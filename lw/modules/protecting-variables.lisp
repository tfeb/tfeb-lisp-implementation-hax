;;;; Protecting variables
;;;
;;; I'd like this to work in more than one implementation, but it
;;; needs MAP-ENVIRONMENT to find variables, and for instance SBCL
;;; does not seem to have an implementation of anything like that.  So
;;; for now this is LispWorks-specific code.
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-Lispworks
  (error "Not LispWorks"))

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.collecting :compile t))

(defpackage :org.tfeb.lw.protecting-variables
  (:use :cl :hcl)
  (:use :org.tfeb.hax.collecting)
  (:export
   #:protecting-variables
   #:*variable-protection-enabled*
   #:variable-protection-error
   #:variable-protection-style-warning
   #:variable-protection-style-warning-name
   #:*variable-protection-compile-time-error*))

(in-package :org.tfeb.lw.protecting-variables)

;;; Portability shims
;;;

(defun map-environment-variables (f e)
  #+LispWorks
  (system:map-environment e :variable f)
  #-LispWorks
  (error "no"))

(defun copy-variable-declarations (from to e)
  #+LispWorks
  (mapcar (lambda (e)
            (destructuring-bind (what . value) e
              `(,what ,value ,to)))
          (nth-value 2 (variable-information from e)))
  #-LispWorks
  '())

(defun variable-ignored-p (v e)
  #+Lispworks
  (cdr (assoc 'ignore (nth-value 2 (variable-information v e))))
  #-LispWorks
  nil)

(define-condition variable-protection-error (cell-error simple-condition)
  ()
  (:documentation "Attempt to assign to a protected variable

This is signalled at run-time and at compile-time if
*VARIABLE-PROTECTION-COMPILE-TIME-ERROR* is true."))

(define-condition variable-protection-style-warning (style-warning simple-condition)
  ((name :reader variable-protection-style-warning-name
         :initarg :name))
  (:documentation "Warning about potential assignmend to a protected variable

This is signalled at compile-time if
 *VARIABLE-PROTECTION-COMPILE-TIME-ERROR* is false."))

(defvar *variable-protection-compile-time-error* nil
  "Should variable protection violations be errors at compile time?")

;;; The purpose of the fillowing functions is to be compiled away by
;;; the corresponding compiler macros but to allow interpreted code to
;;; work.
;;;

(defun id (x v)
  (declare (ignore v))
  x)

(define-compiler-macro id (x v)
  (declare (ignore v))
  x)

(defun (setf id) (n x v)
  (declare (ignore n x))
  (error 'variable-protection-error
         :name v
         :format-control "variable ~S is protected"
         :format-arguments (list v)))

(define-compiler-macro (setf id) (&whole form nf xf vf)
  (declare (ignore nf xf))
  (if (and (consp vf) (eql (car vf) 'quote))
      (let ((v (second vf)))
        (if *variable-protection-compile-time-error*
            (error 'variable-protection-error
                   :name v
                   :format-control "variable ~S is protected"
                   :format-arguments (list v))
          (warn 'variable-protection-style-warning
                :name v
                :format-control "variable ~S is protected"
                :format-arguments (list v))))
    (warn "unexpected strange call to (setf id)?"))
  form)

(defun idf (f v)
  (declare (ignore v)
           (optimize speed))
  (funcall f))

(define-compiler-macro idf (f v)
  (declare (ignore v))
  (if (and (consp f) (eql (first f) 'lambda) (null (second f)))
      (destructuring-bind (lambda null . decls/forms) f
        (declare (ignore lambda null))
        `(locally ,@decls/forms))
    `(funcall ,f)))

(defun (setf idf) (n f v)
  (declare (ignore n f))
  (error 'variable-protection-error
         :name v
         :format-control "symbol-macro ~S is protected"
         :format-arguments (list v)))

(define-compiler-macro (setf idf) (&whole form nf ff vf)
  (declare (ignore nf ff))
  (if (and (consp vf) (eql (car vf) 'quote))
      (let ((v (second vf)))
        (if *variable-protection-compile-time-error*
            (error 'variable-protection-error
                   :name v
                   :format-control "symbol-macro ~S is protected"
                   :format-arguments (list v))
          (warn 'variable-protection-style-warning
                :name v
                :format-control "symbol-macro ~S is protected"
                :format-arguments (list v))))
    (warn "unexpected strange call to (setf idf)?"))
  form)

(defvar *variable-protection-enabled* t
  "If true variable protection is enabled")

(defmacro protecting-variables ((&key (only t) (except '())
                                      (lexical t) (special nil) (symbol-macros nil)
                                      (filter (constantly t))
                                      (enabled *variable-protection-enabled*))
                                &body forms &environment environment)
  ;; Because of the way symbol-macros are dealt with you will likely
  ;; get warnings about ignored ones.  I don't think anything can be
  ;; done about that.
  "Protect local variables from assignment

ONLY should be either a list of variable names or a true value.  If it
is a list only those variables will be protected, if true all
variables will be.

EXCEPT is the same as ONLY in the opposite sense.  If a variable is in
both ONLT and EXCEPT then it will not be protected.

LEXICAL means protect lexical bindings: default true.

SPECIAL means protect special bindings: default false.

SYMBOL-MACROS means protect symbol-macros: default false.

FILTER if given should be a designator for a function of two
arguments: variable name and environment.  If it returns true then the
variable will be protected, if it also passes the other tests.

ENABLED, if true, enables variable protection.  Its default is
*VARIABLE-PROTECTION-ENABLED*.

All of these options matter at compile-time, not after that.

Only variables in the current lexical environment are considered: you
can't protect arbitrary special variables, for instance.

Protecting variables has some overhead.  Protecting symbol-macros
probably has quite a large overhead.  When disabled or when variables
are not protected there should be no overhead at all."
  (if enabled
      (multiple-value-bind (variables symbol-macros symbol-macro-expansions)
          (with-collectors (variable symbol-macro expansion)
            (map-environment-variables
             (lambda (name kind info)
               (flet ((? (v what)
                        (typecase what
                          (list
                           (member v what))
                          (t t))))
                 (when (and (? name only) (not (? name except))
                            (not (variable-ignored-p name environment))
                            (funcall filter name environment))
                   (ecase kind
                     (:lexical
                      (when lexical
                        (variable name)))
                     (:special
                      (when special
                        (variable name)))
                     (:symbol-macro
                      (when symbol-macros
                        (symbol-macro name)
                        (expansion info)))))))
             environment))

        (let ((hidden (mapcar (lambda (v)
                                (make-symbol (string v)))
                              variables)))
          `(let ,(mapcar #'list hidden variables)
             (declare ,@(mapcan (lambda (v h)
                                  (copy-variable-declarations v h environment))
                                variables hidden))
             (symbol-macrolet (,@(mapcar (lambda (n h)
                                           `(,n (id ,h ',n)))
                                         variables hidden)
                               ,@(mapcar (lambda (n e)
                                           `(,n (idf (lambda () ,e) ',n)))
                                         symbol-macros symbol-macro-expansions))
               ,@forms))))
    `(progn ,@forms)))

#||
(defun bench (n m)
  ;; The two times should be very close
  (declare (type fixnum n m)
           (optimize speed))
  (macrolet ((timing (&body forms)
               (let ((start (make-symbol "START")))
                 `(let ((,start (get-internal-real-time)))
                    ,@forms
                    (/ (- (get-internal-real-time) ,start)
                       internal-time-units-per-second)))))
    (let ((unprotected
           (timing (dotimes (i n (* n m))
                     (let ((r (random 10))
                           (a 0))
                       (declare (type fixnum r a))
                       (dotimes (j m a)
                         (incf a r))))))
          (protected
           (timing (dotimes (i n (* n m))
                     (let ((r (random 10))
                           (a 0))
                       (declare (type fixnum r a))
                       (protecting-variables (:only (r))
                         (dotimes (j m a)
                           (incf a r))))))))
      (values (float (/ protected unprotected))
              protected unprotected))))
||#
