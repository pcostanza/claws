(in-package :claws)

#|
   Parameters here are set for full speed by default.
   If you want to debug, say (pushnew :claws-debug *features*) before loading/compiling with asdf.
|#

(defparameter *optimization*
  #+claws-debug '(optimize (speed 0) (space 0) (safety 3) (debug 3) (compilation-speed 0) (float 3))
  #-claws-debug '(optimize (speed 3) (space 0) (safety 0) (debug 0) (compilation-speed 0) (float 0)))

(defparameter *inline*
  #+claws-debug 'notinline
  #-claws-debug 'inline)
