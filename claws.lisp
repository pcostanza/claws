(in-package :claws)

(defun ensure-process-killed (process)
  (when (mp:process-alive-p process)
    (mp:process-kill process)
    (mp:process-wait "process kill" (complement #'mp:process-alive-p) process)))

(defmacro fetch/clear (place &optional default)
  (let ((temp (copy-symbol 'temp)))
    `(let ((,temp ,place))
       (setf ,place ,default)
       ,temp)))

(defconstant +initial-thunk-vector-size+ #.(expt 2 12))

(deftype thunk () '(function ()))
(deftype thunk-vector () '(array thunk (*)))

(defun invalid-thunk ()
  (error "invalid function call"))

(defun make-thunk-vector (size)
  (make-array size :element-type 'thunk :initial-element #'invalid-thunk :single-thread t))

(declaim (ftype (function (thunk-vector integer) thunk) cvref cvref/clear)
         (ftype (function (thunk thunk-vector integer) thunk) (setf cvref))
         (#.*inline* cvref cvref/clear (setf cvref)))

(defun cvref (vector index)
  (declare (thunk-vector vector) (integer index) #.*optimization*)
  (let ((mindex (nth-value 1 (truncate index (length vector)))))
    (declare (fixnum mindex))
    (aref vector mindex)))

(defun cvref/clear (vector index)
  (declare (thunk-vector vector) (integer index) #.*optimization*)
  (let ((mindex (nth-value 1 (truncate index (length vector)))))
    (declare (fixnum mindex))
    (fetch/clear (aref vector mindex))))

(defun (setf cvref) (new-value vector index)
  (declare (thunk-vector vector) (integer index) #.*optimization*)
  (let ((mindex (nth-value 1 (truncate index (length vector)))))
    (declare (fixnum mindex))
    (setf (svref vector mindex) new-value)))

(declaim (ftype (function (thunk-vector integer integer) thunk-vector) grow))

(defun grow (old-vector top bottom)
  (declare (thunk-vector old-vector) (integer top bottom) #.*optimization*)
  (loop initially (assert (not (= top bottom)))
        with new-vector of-type thunk-vector = (make-thunk-vector (ash (length old-vector) 1))
        with old-start of-type fixnum = (nth-value 1 (truncate top (length old-vector)))
;       with old-end   of-type fixnum = (nth-value 1 (truncate bottom (length old-vector)))
        with new-start of-type fixnum = (nth-value 1 (truncate top (length new-vector)))
        with new-end   of-type fixnum = (nth-value 1 (truncate bottom (length new-vector)))
        for old-index of-type fixnum from old-start
        for new-index of-type fixnum from new-start
        until (= new-index new-end) do
        (when (= old-index (length old-vector)) (setq old-index 0))
        (when (= new-index (length new-vector)) (setq new-index 0))
        (setf (aref new-vector new-index)
              (fetch/clear (aref old-vector old-index)))
        finally (return new-vector)))

#|
(defun test-grow ()
  (let* ((size (1+ (random 1000)))
         (stop (if (= size 1) 1
                 (1+ (random (1- size)))))
         (start (random stop))
         (vector (make-thunk-vector size)))
    (loop for i from start below stop do
          (let ((i i)) (setf (cvref vector i) (lambda () i))))
    (let ((copy (copy-seq vector))
          (new-vector (grow vector start stop)))
      (assert (loop for i from start below stop
                    always (eq (cvref copy i) (cvref new-vector i)))))))
|#

(defstruct (q (:constructor makeq
               (&optional (vector-size +initial-thunk-vector-size+) &aux
                          (active-vector (make-thunk-vector vector-size)))))
  (lock (mp:make-lock :name "q-lock" :safep nil) :type mp:lock :read-only t)
  (top 0 :type integer)
  (bottom 0 :type integer)
  (active-vector #() :type thunk-vector))

(defmethod print-object ((q q) stream)
  (print-unreadable-object (q stream :identity t :type t)
    (format stream ":LOCK ~S :TOP ~S :BOTTOM ~S :ACTIVE-VECTOR ~S"
            (q-lock q)
            (q-top q)
            (q-bottom q)
            (length (q-active-vector q)))))

(defglobal-variable *main-q*)
(defglobal-variable *helper-processes*)

(declaim (ftype (function (q thunk)) push-bottom))

(defun push-bottom (q thunk)
  (declare (q q) (thunk thunk) #.*optimization*)
  (mp:with-lock ((q-lock q) "push-bottom")
    (let* ((top (q-top q))
           (bottom (q-bottom q))
           (vector (q-active-vector q))
           (size (- bottom top)))
      (declare (integer top bottom) (thunk-vector vector) (fixnum size))
      (cond ((and (zerop size) (eq q *main-q*))
             (mapc 'mp:process-poke *helper-processes*))
            ((= size (length vector))
             (setf vector (grow vector top bottom))
             (setf (q-active-vector q) vector)))
      (setf (q-bottom q) (1+ bottom))
      (setf (cvref vector bottom) thunk))))

(declaim (ftype (function (q) (or thunk null)) pop-bottom))

(defun pop-bottom (q)
  (declare (q q) #.*optimization*)
  (mp:with-lock ((q-lock q) "pop-bottom")
    (let* ((top (q-top q))
           (bottom (q-bottom q))
           (vector (q-active-vector q))
           (size (- bottom top)))
      (declare (integer top bottom) (thunk-vector vector) (fixnum size))
      (when (> size 0)
        (let ((new-bottom (1- bottom)))
          (declare (integer new-bottom))
          (setf (q-bottom q) new-bottom)
          (cvref/clear vector new-bottom))))))

(declaim (ftype (function (q) (or thunk null)) steal))

(defun steal (q)
  (declare (q q) #.*optimization*)
  (mp:with-lock ((q-lock q) "steal" 0)
    (let* ((top (q-top q))
           (bottom (q-bottom q))
           (vector (q-active-vector q))
           (size (- bottom top)))
      (declare (integer top bottom) (thunk-vector vector) (fixnum size))
      (when (> size 0)
        (setf (q-top q) (1+ top))
        (cvref/clear vector top)))))


(defglobal-variable *all-queues*)

(defvar *current-q*)
(defvar *randomq-state* (make-random-state t))

(defun randomq ()
  (svref *all-queues* (random (length *all-queues*) *randomq-state*)))

(declaim (inline make-join-context))

(defstruct (join-context (:constructor make-join-context ()))
  (started-tasks 0 :type fixnum)
  (stopped-tasks 0 :type fixnum))

(defvar *join-context*)

(declaim (ftype (function (thunk)) fork)
         (#.*inline* fork))

(defun fork (thunk)
  (declare (thunk thunk) #.*optimization*)
  (let ((join-context *join-context*))
    (declare (join-context join-context))
    (incf (join-context-started-tasks join-context))
    (push-bottom *current-q* (lambda ()
                               (unwind-protect (funcall thunk)
                                 (sys:atomic-fixnum-incf
                                  (join-context-stopped-tasks join-context)))))))

(defmacro spawn ((&rest vars) &body body)
  `(flet ((spawn-function ()
            (multiple-value-setq ,vars
                (let ((*join-context* (make-join-context)))
                  (declare (dynamic-extent *join-context*))
                  ,@body))))
     (declare (#.*inline* spawn-function) #.*optimization*)
     (fork #'spawn-function)
     (values)))

(defun sync ()
  (declare #.*optimization*)
  (loop with join-context of-type join-context = *join-context*
        with current-q of-type q = *current-q*
        with thunk = t
        until (= (the fixnum (join-context-started-tasks join-context))
                 (the fixnum (join-context-stopped-tasks join-context)))
        when (setq thunk (if thunk 
                           (pop-bottom current-q)
                           (steal (randomq))))
        do (funcall (the thunk thunk))
        finally (return (values))))

(defmacro parlet ((&rest bindings) &body body)
  `(let ,(loop for (vars) in bindings
               if (symbolp vars) collect vars
               else append vars)
     ,@(loop for ((vars form) . more) on bindings
             if more collect `(spawn ,(if (symbolp vars) (list vars) vars) ,form)
             else collect `(,(if (symbolp vars) 'setq 'multiple-value-setq) ,vars ,form))
     (sync)
     (locally ,@body)))

(defmacro seqlet ((&rest bindings) &body body)
  (if bindings
    `(multiple-value-bind ,@(car bindings)
         (seqlet ,(cdr bindings) ,@body))
    `(locally ,@body)))

(defglobal-parameter *worker-thread-name* "claws-worker-thread")

(defun poll-work (current-q)
  (let ((*current-q* current-q)
        (*randomq-state* (make-random-state t))
        (pause 1.0E-9))
    (flet ((make-steal-attempts ()
             (loop repeat (length *all-queues*)
                   when (steal (randomq)) return it)))
      (tagbody
       :pop-local (when-let (thunk (pop-bottom current-q))
                    (funcall (the thunk thunk))
                    (go :pop-local))
       :steal     (when-let (thunk (make-steal-attempts))
                    (funcall (the thunk thunk))
                    (go :pop-local))
       :pause     (if (mp:current-process-pause pause)
                    (setq pause 1.0E-9)
                    (setq pause (* 2.0E0 pause)))
       (go :steal)))))

(defun reset-workers (nof-processes)
  (loop for process in (mp:list-all-processes)
        when (string= (mp:process-name process) *worker-thread-name*)
        do (ensure-process-killed process))
  (loop with all-queues = (make-array nof-processes :single-thread t)
        for i below nof-processes do (setf (svref all-queues i) (makeq))
        finally
        (setq *all-queues* all-queues)
        (setq *current-q* (svref all-queues 0))
        (setq *join-context* (make-join-context))
        (setq *main-q* *current-q*)
        (setq *helper-processes*
              (loop for i from 1 below nof-processes
                    collect (mp:process-run-function *worker-thread-name* '() 'poll-work (svref *all-queues* i)))))
  :ready)

(reset-workers 1)
