(in-package :claws)

(declaim (inline list-all-processes))

(defun list-all-processes ()
  #+lispworks (mp:list-all-processes)
  #+sbcl (sb-thread:list-all-threads))

(declaim (inline process-run))

(defun process-run (name function &rest arguments)
  (declare (dynamic-extent arguments))
  #+lispworks (apply 'mp:process-run-function name '() function arguments)
  #+sbcl (sb-thread:make-thread function :name name :arguments (copy-list arguments)))

(declaim (inline process-name))

(defun process-name (process)
  #+lispworks (mp:process-name process)
  #+sbcl (sb-thread:thread-name process))

(defun ensure-process-killed (process)
  #+lispworks
  (when (mp:process-alive-p process)
    (mp:process-kill process)
    (mp:process-wait "process kill" (complement #'mp:process-alive-p) process))
  #+sbcl
  (when (sb-thread:thread-alive-p process)
    (sb-thread:terminate-thread process)
    (sb-thread:join-thread process :default nil)))

(deftype lock ()
  #+lispworks 'mp:lock
  #+sbcl 'sb-thread:mutex)

(declaim (inline make-lock))

(defun make-lock (name)
  #+lispworks (mp:make-lock :name name :safep nil)
  #+sbcl (sb-thread:make-mutex :name name))

(defmacro with-lock ((lock &optional (whostate nil whostatep) (timeout nil timeoutp)) &body body)
  (declare (ignorable whostate))
  #+lispworks
  (cond (timeoutp  `(mp:with-lock (,lock ,whostate ,timeout) ,@body))
        (whostatep `(mp:with-lock (,lock ,whostate) ,@body))
        (t         `(mp:with-lock (,lock) ,@body)))
  #+sbcl
  (if timeoutp
    (let ((timeoutv (copy-symbol 'timeout)))
      `(let ((,timeoutv ,timeout))
         (sb-thread:with-recursive-lock (,lock :wait-p (> ,timeoutv 0) :timeout ,timeoutv) ,@body)))
    `(sb-thread:with-recursive-lock (,lock) ,@body)))

(deftype atomic-int ()
  #+lispworks 'fixnum
  #+sbcl 'sb-ext:word)

(defmacro atomic-incf (place)
  #+lispworks
  `(sys:atomic-fixnum-incf ,place)
  #+sbcl
  `(sb-ext:atomic-incf ,place))

(defmacro defglobal (var &optional (value nil valuep))
  #+lispworks (if valuep
                `(hcl:defglobal-variable ,var ,value)
                `(hcl:defglobal-variable ,var))
  #+sbcl `(sb-ext:defglobal ,var ,value))

(defmacro fetch/clear (place &optional default)
  (let ((temp (copy-symbol 'temp)))
    `(let ((,temp ,place))
       (setf ,place ,default)
       ,temp)))

(defconstant +initial-thunk-vector-size+ #.(expt 2 12))

(deftype thunk () '(function ()))
(deftype thunk-vector ()
  #+lispworks '(array thunk (*))
  #+sbcl 'simple-vector)

(defun invalid-thunk ()
  (error "invalid function call"))

(defun make-thunk-vector (size)
  (make-array size 
              #+lispworks :element-type #+lispworks 'thunk
              #+lispworks :single-thread #+lispworks t
              :initial-element #'invalid-thunk))

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
    (setf (aref vector mindex) new-value)))

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

(defstruct (q (:constructor makeq
               (&optional (vector-size +initial-thunk-vector-size+) &aux
                          (active-vector (make-thunk-vector vector-size)))))
  (lock (make-lock "q-lock") :type lock :read-only t)
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

(defglobal *main-q*)
(defglobal *helper-processes*)

(declaim (ftype (function (q thunk)) push-bottom))

(defun push-bottom (q thunk)
  (declare (q q) (thunk thunk) #.*optimization*)
  (with-lock ((q-lock q) "push-bottom")
    (let* ((top (q-top q))
           (bottom (q-bottom q))
           (vector (q-active-vector q))
           (size (- bottom top)))
      (declare (integer top bottom) (thunk-vector vector) (fixnum size))
      (cond #+lispworks
            ((and (zerop size) (eq q *main-q*))
             (mapc 'mp:process-poke *helper-processes*))
            ((= size (length vector))
             (setf vector (grow vector top bottom))
             (setf (q-active-vector q) vector)))
      (setf (q-bottom q) (1+ bottom))
      (setf (cvref vector bottom) thunk))))

(declaim (ftype (function (q) (or thunk null)) pop-bottom))

(defun pop-bottom (q)
  (declare (q q) #.*optimization*)
  (with-lock ((q-lock q) "pop-bottom")
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
  (with-lock ((q-lock q) "steal" 0)
    (let* ((top (q-top q))
           (bottom (q-bottom q))
           (vector (q-active-vector q))
           (size (- bottom top)))
      (declare (integer top bottom) (thunk-vector vector) (fixnum size))
      (when (> size 0)
        (setf (q-top q) (1+ top))
        (cvref/clear vector top)))))


(defglobal *all-queues*)

(defvar *current-q*)
(defvar *randomq-state* (make-random-state t))

(defun randomq ()
  (svref *all-queues* (random (length *all-queues*) *randomq-state*)))

(declaim (inline make-join-context))

(defstruct (join-context (:constructor make-join-context ()))
  (started-tasks 0 :type atomic-int)
  (stopped-tasks 0 :type atomic-int))

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
                                 (atomic-incf (join-context-stopped-tasks join-context)))))))

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
        until (= (the atomic-int (join-context-started-tasks join-context))
                 (the atomic-int (join-context-stopped-tasks join-context)))
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

(defglobal *worker-thread-name* "claws-worker-thread")

(defun poll-work (current-q)
  (let ((*current-q* current-q)
        (*randomq-state* (make-random-state t))
        #+lispworks (pause 1.0E-9))
    (flet ((make-steal-attempts ()
             (loop #+lispworks repeat #+lispworks (length *all-queues*)
                   when (steal (randomq)) return it)))
      (tagbody
       :pop-local  (let ((thunk (pop-bottom current-q)))
                     (when thunk
                       (funcall (the thunk thunk))
                       (go :pop-local)))
       :steal      (let ((thunk (make-steal-attempts)))
                     (when thunk
                       (funcall (the thunk thunk))
                       (go :pop-local)))
       #+lispworks (if (mp:current-process-pause pause)
                     (setq pause 1.0E-9)
                     (setq pause (* 2.0E0 pause)))
       (go :steal)))))

(defun reset-workers (nof-processes)
  (loop for process in (list-all-processes)
        when (string= (process-name process) *worker-thread-name*)
        do (ensure-process-killed process))
  (loop with all-queues = (make-array nof-processes #+lispworks :single-thread #+lispworks t)
        for i below nof-processes do (setf (svref all-queues i) (makeq))
        finally
        (setq *all-queues* all-queues)
        (setq *current-q* (svref all-queues 0))
        (setq *join-context* (make-join-context))
        (setq *main-q* *current-q*)
        (setq *helper-processes*
              (loop for i from 1 below nof-processes
                    collect (process-run *worker-thread-name* 'poll-work (svref *all-queues* i)))))
  :ready)

(reset-workers 1)
