;;;; Stack extension control for LW
;;;

#-LispWorks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "LispWorks only"))

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.metatronic :compile t))

(defpackage :org.tfeb.lw.allowing-stack-extensions
  (:use :cl :org.tfeb.hax.metatronic)
  (:import-from :conditions #:stack-overflow)
  (:import-from :hcl #:current-stack-length)
  (:export
   #:*stack-limit*
   #:allowing-stack-extensions))

(in-package :org.tfeb.lw.allowing-stack-extensions)

(provide :org.tfeb.lw.allowing-stack-extensions)

(defvar *stack-limit* (current-stack-length)
  "How big the stack is allowed to get by default.

This should be a real, T (always extend), or NIL (never extend).")

(defmacro/m allowing-stack-extensions ((&key (limit '*stack-limit* limitp)
                                             (use-stack-limit nil))
                                       &body forms)
  "Control stack extensions in LW.

If LIMIT is given this is the limit (see *STACK-LIMIT* for what it can
be).  If USE-STACK-LIMIT is given as true, then the dynamic value of
*STACK-LIMIT* will be used instead: this overrides any LIMIT value."
  ;; It is tempting to allow the limit to be a predicate, but I think
  ;; calling functions at this point is probably not a good idea.
  (when (and use-stack-limit limitp)
    (warn "both LIMIT and USE-STACK-LIMIT given: will use *STACK-LIMIT*"))
  (if use-stack-limit
      `(handler-bind ((stack-overflow
                       (lambda (c)
                         (when (or (eq *stack-limit* t)
                                   (< (current-stack-length) *stack-limit*))
                           (let ((r (find-restart 'continue c)))
                             (when r (invoke-restart r)))))))
         ,@forms)
    `(let ((<limit> ,limit))
       (check-type <limit> (or boolean real))
       (handler-bind ((stack-overflow
                       (lambda (c)
                         (when (or (eq <limit> t)
                                   (< (current-stack-length) <limit>))
                           (let ((r (find-restart 'continue c)))
                             (when r (invoke-restart r)))))))
         ,@forms))))
