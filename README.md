claws
=====

CLAWS - Work-stealing scheduler for LispWorks

CLAWS is a simple work-stealing scheduler for LispWorks, to support fork/join
computations for parallel programming. It is inspired by other work-stealing
schedulers, such as those of Cilk and Threading Building Blocks, and provides a
simple parallel programming model that works well in many cases. Through work
stealing, it automatically balances the computational load over several worker
threads, which ensures good performance even for irregular computations that
are hard to schedule statically.

The API consists of two essential functions: 'fork and 'sync.

Fork takes a thunk (a function without parameters) and places it on a
thread-local queue. It will subsequently be either stolen by some worker thread
and executed in parallel, or else eventually be executed by the current thread.

Return values from such thunks are ignored. If you need to return results from
parallel computations, the forked thunks need to close over some variables in
the lexical variables and store their results there, or use global variables to
communicate results.

Sync is used for synchronization. It ensures that previously spawned thunks
have finished their execution, so their results, if any, can be safely used.

To make it easier to deal with results from parallel computations, there is
also a convenience macro 'spawn. Spawn takes a list of variable names and a
body of code. It converts the code body into a thunk that sets the given
variable names to the result(s) of executing that code body. (For multiple
variables, the code body must return the results as multiple values.)

Another convenience macro 'parlet can be used a binding form for parallel
computations. It also deals with potential multiple return values, and 'seqlet
is provided as an equivalent sequential binding form that can be used
interchangeably. See the Fibonacci example that illustrates the use of 'parlet
and 'seqlet.

Finally, 'reset-workers is a function that tells the runtime how many threads
the work-stealing scheduler should use to execute parallel computations. By
default, only one thread is assigned, which means that you get (very
inefficient) single-threaded, sequential execution. To use more than one
thread, 'reset-workers needs to be called with the total number of threads,
including the current main thread.

Reset-workers must not be called while a parallel computation is currently
being executed, so it's best to call it once at the beginning of an
application. However, it's safe to call 'reset-workers while the task queues
are all empty. It will kill all current helper threads, and then create new
ones according to the requested total number of threads.

Helper threads will continuously look for work to steal from other threads.
However, if they don't find any work, they will pause in order not to keep
threads busy while other applications may have better uses for them. However,
during parallel computations, expect that most to all worker threads will be
busy, so be careful with picking the right number of worker threads, especially
for desktop applications.

When using CLAWS to map a function over several elements in parallel, it's best
to use a tree-recursive form rather than forking/spawning the function for each
single element. Here is a rough sketch how to express such tree-recursive
functions:

<pre>
(defun parallel-for-each (function vector &optional (threshold 10))
  (labels ((recur (start end &aux (length (- end start)))
             (if (&lt; length threshold)
               (loop for i from start below end
                     do (funcall function (aref vector i)))
               (let* ((half (floor length 2))
                      (mid (+ start half)))
                 (spawn () (recur start mid))
                 (recur mid end)
                 (sync)))))
    (recur 0 (length vector))))
</pre>

It's also a good idea to find a threshold below which an algorithm switches to
sequential execution, since scheduling incurs an overhead, and tasks need to
have a minimum execution cost before it pays off to schedule them to parallel
threads.
