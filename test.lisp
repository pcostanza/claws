(use-package :claws)

(defun sfib (n)
  (declare #.claws::*optimization*)
  (if (< n 2) n
    (seqlet (((f-1) (sfib (- n 1)))
             ((f-2) (sfib (- n 2))))
      (+ f-1 f-2))))

(defun fib (n) 
  (declare #.claws::*optimization*)
  (if (< n 20) 
    (sfib n)
    (parlet (((f-1) (fib (- n 1)))
             ((f-2) (fib (- n 2))))
      (+ f-1 f-2))))
