;;;; Recording advice for LW
;;;
;;; CAUTION: may explode if dropped; hypergolic in air above 200K.
;;;

#-LispWorks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "LispWorks only"))

(defpackage :org.tfeb.lw.recording-advice
  (:add-use-defaults)
  (:export #:map-recorded-advice
   #:*recording-advice*))

(in-package :org.tfeb.lw.recording-advice)

(provide :org.tfeb.lw.recording-advice)

(defparameter *recording-advice* nil ;parameter so it is unilaterally off
  "If true, record advice definitinons for later mapping with MAP-ADVISED.")

(defvar *recorded-advices* '())

(defadvice (compiler::define-around-advice recording-advice :after)
    (what name &rest junk)
  (declare (ignore junk))
  (when *recording-advice*
    (pushnew (cons what name) *recorded-advices* :test #'equal)))

(defadvice (compiler::define-before-advice recording-advice :after)
    (what name &rest junk)
  (declare (ignore junk))
  (when *recording-advice*
    (pushnew (cons what name) *recorded-advices* :test #'equal)))

(defadvice (compiler::define-after-advice recording-advice :after)
    (what name &rest junk)
  (declare (ignore junk))
  (when *recording-advice*
    (pushnew (cons what name) *recorded-advices* :test #'equal)))

(defadvice (remove-advice recording-advice :after) (what name)
  ;; Do this even when not recording so the list never includes things
  ;; which are not advised.
  (setf *recorded-advices* (delete (cons what name) *recorded-advices* :test #'equal)))

(defun map-recorded-advice (f &optional (inspect-only nil))
  "Map a function over advised definitions for side-effect.

The function is called with two arguments: the dspec of the definition
and the name of the advice.  The function is allowed to remove the
advice.  Optional second argument, INSPECT-ONLY, if true, asserts that
the function will *not* remove or add any advice.

The mapping happens in the reverse order to that in which advice was
originally defined: that's usually the right order for removing advice.

Return NIL."
  (dolist (e (if inspect-only *recorded-advices* (copy-list *recorded-advices*)) nil)
    (funcall f (car e) (cdr e))))
