;;;; Replayable advice for LW
;;;
;;; CAUTION: may explode if dropped; hypergolic in air above 200K.
;;;

#-LispWorks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "LispWorks only"))

(defpackage :org.tfeb.lw.replayable-advice
  (:add-use-defaults)
  (:export
   #:map-replayable-advice
   #:remove-replayable-advice
   #:*replayable-advice*))

(in-package :org.tfeb.lw.replayable-advice)

(provide :org.tfeb.lw.replayable-advice)

(defparameter *replayable-advice* nil   ;unilaterally off
  "If true record advice in such a way it can be replayed")

(defvar *replayable-advices* '())

(defvar *replaying-advice* nil)

(defadvice (compiler::define-around-advice replayable-advice :after)
    (what name &rest args)
  (when (and *replayable-advice* (not *replaying-advice*))
    (let ((match (assoc (cons what name) *replayable-advices* :test #'equal))
          (replay (lambda ()
                    (let ((*replaying-advice* t))
                      (apply #'compiler::define-around-advice
                             what name args)))))
      (if match
          (setf (cdr match) replay)
        (push (cons (cons what name) replay) *replayable-advices*)))))

(defadvice (compiler::define-before-advice replayable-advice :after)
    (what name &rest args)
  (when (and *replayable-advice* (not *replaying-advice*))
    (let ((match (assoc (cons what name) *replayable-advices* :test #'equal))
          (replay (lambda ()
                    (let ((*replaying-advice* t))
                      (apply #'compiler::define-before-advice
                             what name args)))))
      (if match
          (setf (cdr match) replay)
        (push (cons (cons what name) replay) *replayable-advices*)))))

(defadvice (compiler::define-after-advice replayable-advice :after)
    (what name &rest args)
  (when (and *replayable-advice* (not *replaying-advice*))
    (let ((match (assoc (cons what name) *replayable-advices* :test #'equal))
          (replay (lambda ()
                    (let ((*replaying-advice* t))
                      (apply #'compiler::define-after-advice
                             what name args)))))
      (if match
          (setf (cdr match) replay)
        (push (cons (cons what name) replay) *replayable-advices*)))))

(defun map-replayable-advice (f)
  "Map a function over replayable advices

The function is called with three arguments: the advised name, the
name of the advice, and a function which, if called, will replay the
advice.

The function can add or remove any advice as well as replaying it.
See also REMOVE-REPLAYABLE-ADVICE.

The mapping takes place in the order advice was first defined.

Return NIL"
  (dolist (r (reverse *replayable-advices*) nil)
    (destructuring-bind ((what .  name) . replayer) r
      (funcall f what name replayer))))

(defun remove-replayable-advice (what name)
  "Remove replayable advice for WHAT and NAME"
  (let ((wn (cons what name)))
    (if (assoc wn *replayable-advices* :test #'equal)
        (progn
          (setf *replayable-advices*
                (delete wn *replayable-advices*
                               :key #'car :test #'equal)))
      nil)))
