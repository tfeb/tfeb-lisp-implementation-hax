;;;; LW commands
;;; $Id$

;;; lw-commands.lisp is copyright 2002, 2012, 2020, 2021, 2022 by me,
;;; Tim Bradshaw, and may be used for any purpose whatsoever by
;;; anyone. It has no warranty whatsoever. I would appreciate
;;; acknowledgement if you use it in anger, and I would also very much
;;; appreciate any feedback or bug fixes.
;;;
;;; This file expects it is loaded at image start: it is not dumped
;;; with the image.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "asdf")
  #-org.tfeb.tools.require-module
  (error "need require-module"))

(defpackage :org.tfeb.lw.lw-commands
  (:add-use-defaults t)
  (:use :org.tfeb.tools.require-module)
  (:export
   #:declare-extra-lw-commands
   #:*prompts*))

(in-package :org.tfeb.lw.lw-commands)

(provide-module :org.tfeb.lw.lw-commands)
(pushnew :org.tfeb.lw.lw-commands *features*)

;;;; Declaring extra LW commands
;;; LW now has facilities to do this via
;;; SYSTEM:DEFINE-TOP-LOOP-COMMAND.  *However* that does not allow you
;;; to supply documentation strings for commands you add, which makes
;;; :? less useful.  So I am still using this ancient and undocumented
;;; hack, for now.
;;;

(defvar *extra-lw-commands* '())

(defmacro declare-extra-lw-commands (&body command-forms)
  `(loop for c in ',command-forms
         do (let ((found (assoc (car c) *extra-lw-commands*)))
              (if found
                  (setf (cdr found) (cdr c))
                (setf *extra-lw-commands*
                      (nconc *extra-lw-commands* (list c)))))
         finally (return (values))))

(defadvice (sys::%top-level extra-commands :around)
    (&rest args &key (extra-commands nil) &allow-other-keys)
  (if (null extra-commands)
      ;; I suppose this is OK, though I'm not sure it should be!
      (apply #'call-next-advice :extra-commands *extra-lw-commands*
             args)
    (apply #'call-next-advice :extra-commands (append *extra-lw-commands*
                                                      extra-commands)
           args)))

;;;; Package stack
;;;

(defvar *package-stack* '())

(defvar *last-package* *package*)

;;; must bind per process
(pushnew '(*package-stack*) mp:*process-initial-bindings*
         :test #'equal)
(pushnew '(*last-package* . *package*) mp:*process-initial-bindings*
         :test #'equal)

(defun package-command (cmd &rest args)
  (case cmd
    ((:pkg>)
     (when (or (null args) (not (null (cdr args))))
       (warn "expecting a package name, got ~S" args)
       (return-from package-command (values)))
     (destructuring-bind (p/n) args
       (let ((p (find-package p/n)))
         (when (null p)
           (warn "no package ~S" p/n)
           (return-from package-command (values)))
         (push *package* *package-stack*)
         (setf *last-package* *package*
               *package* p))))
    ((:pkg~)
     (unless (null (rest args))
       (warn "ignoring spurious arguments"))
     (when (null *package-stack*)
       (warn "empty stack")
       (return-from package-command (values)))
     (let ((pkg *package*))
       ;; this stops it setting *PACKAGE* even if ROTATEF fails
       (rotatef pkg (nth (or (first args) 0) *package-stack*))
       (setf *last-package* *package*
             *package* pkg)))
    ((:pkg-)
     (unless (null (rest args))
       (warn "ignoring spurious arguments"))
     (rotatef *package* *last-package*)
     *package*)
    ((:pkg<)
     (unless (null (rest args))
       (warn "ignoring spurious arguments"))
     (let ((n (or (first args) 1))
           (l (length *package-stack*)))
       (cond
        ((zerop l)
         ;; Nothing to pop
         (warn "empty stack")
         (values))
        ((> n l)
         (warn "drained stack")
         (setf *last-package* *package*
               *package* (nth (1- l) *package-stack*)
               *package-stack* '())
         *package*)
        (t
         (setf *last-package* *package*
               *package* (nth (1- n) *package-stack*)
               *package-stack* (nthcdr n *package-stack*))
         *package*))))
    ((:pkg)
     (cond
      ((null args)
       (format t "~&Current package ~A~%   Last package ~A~%~
                  ~@[  Package stack ~{~<~%              ~:;~A~>~^ ~}~%~]"
               (package-name *package*)
               (package-name *last-package*)
               (mapcar #'package-name *package-stack*))
       (values))
      ((null (cdr args))
       (destructuring-bind (p/n) args
         (let ((p (find-package p/n)))
           (when (null p)
             (warn "no package ~S" p/n)
             (return-from package-command (values)))
           (setf *last-package* *package*
                 *package* p))))
      (t
       (warn "expecting a package name, got ~S" args)
       (values))))
    (otherwise
     (warn "unknown package command ~S" cmd)
     (values))))

(declare-extra-lw-commands
  (:pkg> package-command "Push a package")
  (:pkg< package-command "Pop a package")
  (:pkg~ package-command "Swap packages")
  (:pkg- package-command "Set package to previous package")
  (:pkg package-command "With argument set package, with none show package state"))


;;;; Doing things to files
;;;

(defvar *file-command-default-files '())

(defun file-command (cmd &rest args)
  (let ((files (if args
                   (setf *file-command-default-files args)
                 *file-command-default-files)))
    (case cmd
      ((:ld)
       (dolist (f files (values))
         (load f)))
      ((:cf)
       (dolist (f files (values))
         (compile-file f)))
      ((:cl)
       (dolist (f files (values))
         (let ((cp (compile-file-pathname f))
               (sp (merge-pathnames f (load-time-value
                                       (make-pathname :type "lisp")))))
           (when (or (not (probe-file cp))
                     (< (file-write-date cp) (file-write-date sp)))
             (compile-file f))
           (load cp))))
      (otherwise
       (warn "unknown file command ~S" cmd)
       (values)))))

(declare-extra-lw-commands
  (:ld file-command "Load a file")
  (:cf file-command "Compile a file")
  (:cl file-command "Compile a file and load it"))


;;;; Doing things to systems
;;;

(defvar *system-command-default-args* '())

(defun system-command (cmd &rest args)
  (case cmd
    ((:sysdcl)
     (loop for d in (or args (list (get-working-directory)))
           do
           (if (typep d '(or string pathname))
               (let* ((sysdcl (merge-pathnames
                               d
                               (load-time-value
                                (make-pathname :name "sysdcl"
                                               :type "lisp"))))
                      (sysdclc (compile-file-pathname sysdcl))
                      (sysdclt (and (probe-file sysdcl)
                                    (file-write-date sysdcl)))
                      (sysdclct (and (probe-file sysdcl)
                                     (file-write-date sysdcl))))
                 (if sysdclct
                     (load (if sysdclt
                               (if (> sysdclct sysdclt)
                                   sysdclc
                                 sysdcl)
                             sysdclc))
                   (if sysdclt
                       (load sysdcl)
                     (warn "can't find sysdcl, tried ~A and ~A"
                           sysdcl sysdclc))))
             (warn "~S doesn't look like a pathname" d))))
    ((:lss)
     (unless (null args)
       (warn "ignoring arguments"))
          (format t "~&~{~<~%~:;~A/~A~>~^ ~}~%"
                  (loop for s in (append
                                  (scm:all-systems)
                                  ;; seems to be random order?
                                  (asdf:registered-systems))
                        collect (typecase s
                                  (scm:scm-system "l")
                                  (string "a")
                                  (t "?"))
                        collect (typecase s
                                  (scm:scm-system (string-downcase
                                                   (scm:module-name s)))
                                  (t s)))))
    (otherwise
     (let* ((args (if args
                      (setf *system-command-default-args* args)
                    *system-command-default-args*))
            (system-designator (first args)))
       (unless (not (null args))
         (warn "no arguments and no default arguments")
         (return-from system-command (values)))
       (ecase cmd
         ((:lds :cps)
          (apply (cond ((scm:find-system system-designator nil)
                        (case cmd
                          ((:lds) #'load-system)
                          ((:cps) #'compile-system)))
                       ((asdf:find-system system-designator nil)
                        (case cmd
                          ((:lds) #'asdf:load-system)
                          ((:cps) #'asdf:compile-system)))
                       (t
                        (warn "no system ~S" system-designator)
                        (return-from system-command (values))))
                 args))))))
  (values))

(declare-extra-lw-commands
  (:sysdcl system-command "Load sysdcl files")
  (:lss system-command "List known systems")
  (:cps system-command "Compile a system")
  (:lds system-command "Load a system"))


;;;; Inspector
;;;

(defun gi (cmd &rest args)
  (case cmd
    ((:gi)
     (let ((*inspect-through-gui* t))
       (declare (special *inspect-through-gui*))
       (cond (args
              (mapc #'(lambda (a)
                        (inspect (eval a)))
                    args))
             ((null /))
             ((null (cdr /))
              (inspect (car /)))
             (t (inspect /))))))
  (values))

(declare-extra-lw-commands
  (:gi gi "Inspect graphically"))


;;;; Background
;;;

(defun bg (cmd &rest forms)
  (case cmd
    ((:&)
     (if forms
         (mp:process-run-function
          (format nil "~{~S~^ ~}" forms)
          '()
          (compile nil `(lambda ()
                          ,@forms)))
       (progn
         (warn "need something to run")
         (values))))))

(declare-extra-lw-commands
  (:& bg "Run in the background"))

;;;; Requiring modules
;;;
;;; All the actual code for this is now in its own module
;;;

(defun require-module-command (cmd &rest args)
  (declare (ignore cmd))
  (destructuring-bind (module/s &rest kws &key (use t) &allow-other-keys) args
    (apply (if (consp module/s)
               #'require-modules
             #'require-module)
           module/s :use use kws)))

(declare-extra-lw-commands
  (:require require-module-command
   "Require modules, doing some fancy searching
         :VERBOSE T means be verbose, :FORGET-SYSTEMS NIL means
         do not forget systems defined when loading.
         :PRETEND T just tells you what it would have done.
         Use the module's package by default (:USE NIL to stop this).
         See ORG.TFEB.TOOLS.REQUIRE-MODULE:REQUIRE-MODULE for details."))


;;;; Directory
;;;
;;; This is now too hairy and, thus, probably buggy.  In particular be
;;; aware that if you have a pathname whose name component names a
;;; directory, then CHANGE-DIRECTORY does the appropriate thing (well,
;;; arguably), but if you then try and merge a pathname with only a
;;; name component with it it will replace the name.  That's the right
;;; behaviour, but it means that you need to make sure pathnames which
;;; should be directories are.  So for instance directory aliases in
;;; files need to end with slashes.
;;;

;;; I don't think these makes sense on a per-thread basis.
;;;
(defvar *directory-stack* '())

(defvar *last-directory* (get-working-directory))

;;; An alist of (alias . translation).  The translation may be another
;;; alias.
(defvar *directory-aliases* '())

(defun load-directory-aliases (into &optional (directory (get-working-directory)))
  ;; Load directory aliases if found.  INTO is the existing aliases
  ;; and may be mutated by this function.  Anything which would
  ;; introduce a loop or a dangling translation is rejected (is this
  ;; right?).  Return two values: the new aliases and whether we
  ;; loaded anything.
  (let ((alias-file (make-pathname :name ".directory-aliases"
                                   :defaults directory)))
    (unless (probe-file alias-file)
      (return-from load-directory-aliases (values into nil)))
    (let ((new-aliases
           (if (probe-file alias-file)
               (with-open-file (in alias-file)
                 (with-standard-io-syntax
                   (let ((*read-eval* nil))
                     (loop for alias = (read in nil in)
                           until (eq alias in)
                           collect alias))))
             '())))
      (unless (and (listp new-aliases)
                   (every (lambda (e)
                            (and (listp e)
                                 (= (length e) 2)
                                 (symbolp (first e))
                                 (typep (second e) '(or symbol string pathname))))
                          new-aliases))
        (warn "bad aliases in ~A" alias-file)
        (return-from load-directory-aliases (values into nil)))
      (dolist (na new-aliases (values into t))
        (destructuring-bind (alias translation) na
          (if (translate-directories (list alias) (acons alias translation into))
              ;; This would translate, so install it
              (let ((found (assoc alias into))
                    (effective-translation (typecase translation
                                             ((or string pathname)
                                              ;; allow relative to load dir
                                              (namestring (merge-pathnames translation
                                                                           directory)))
                                             (symbol translation))))
                (if found
                    (setf (cdr found) effective-translation)
                  (setf into (acons alias effective-translation into))))
            (warn "alias ~S -> ~S fails to translate: ignoring it"
                  alias translation)))))))

(defun translate-directories (directories/aliases
                              &optional (aliases *directory-aliases*))
  ;; Translate a list of directories and aliases into a single
  ;; directory, or fail.  It is important that if there is a single
  ;; entry in the list, and it is not an alias, it is returned as is:
  ;; chdir (below) relies on this to know whether a translation has
  ;; happened.
  (labels ((translate (a seen)
             ;; Translate aliases, handling loops
             (let ((found (cdr (assoc a aliases))))
               (typecase found
                 ((or string pathname)
                  found)
                 (null
                  nil)
                 (symbol
                  (cond
                   ((member found seen)
                    (warn "directory alias loop at ~S~@[ (via ~{~S~^ ~})~]"
                          a (reverse seen))
                    nil)
                   (t
                    (translate found (cons found seen)))))
                 (t
                  (warn "mutant alias ~S -> ~S" a found)
                  nil))))
           (td-loop (ds/as into)
             ;; Translate a list of directories and aliases,
             ;; successively merging into INTO
             (if (null ds/as)
                 (values into nil)
               (destructuring-bind (d/a . more) ds/as
                 (let ((translation (typecase d/a
                                      (symbol
                                       (translate d/a nil))
                                      (list
                                       (td-loop d/a nil))
                                      ((or string pathname character)
                                       d/a)
                                      (t
                                       (warn "what even is this?")
                                       nil))))
                   (if translation
                       (td-loop more (if into
                                         (merge-pathnames translation into)
                                       translation))
                     (values into ds/as)))))))
    (td-loop directories/aliases nil)))

;;; Bootstrap
;;;
(setf *directory-aliases* (load-directory-aliases *directory-aliases*
                                                  (user-homedir-pathname)))

(defun directory-command (cmd &rest args)
  (flet ((show-dirstack (&optional (return nil))
           (cond (return
                  (values (get-working-directory) *directory-stack*))
                 (t
                  (format t "~&~A~@[ ~{~<~% ~:;~A~>~^ ~}~]~%"
                          (get-working-directory) *directory-stack*)
                  (values))))
         (chdir (args)
           ;; Returns true if we did change directory
           (let ((d (get-working-directory))
                 (did-cd t)
                 (l (length args)))
             (cond
              ((= l 0)
               (change-directory (user-homedir-pathname)))
              ((and (= l 1)
                    (typep (first args) '(or symbol string character))
                    (string= (string (first args)) "-"))
               (change-directory *last-directory*)
               (format t "~&~A~%" (get-working-directory)))
              ((and (= l 1)
                    (typep (first args) '(or symbol string character))
                    (string= (string (first args)) "="))
               ;; This used to be "*" for a bit: I want a name
               ;; which refers to an already interned symbol in
               ;; CL, so not "?", sadly.
               (format t "~&~:{~S -> ~S~%~}" (mapcar (lambda (a)
                                                       (list (car a) (cdr a)))
                                                     *directory-aliases*))
               (setf did-cd nil))
              (t
               (labels ((translate-and-load (dirs/aliases)
                          (multiple-value-bind (translation more)
                              (translate-directories dirs/aliases)
                            (cond
                             (translation
                              (when (or (> l 1)
                                        (not (eq translation (first dirs/aliases))))
                                (format t "~&~{~S~^ ~} -> ~A~%"
                                        (ldiff dirs/aliases more) translation))
                              (cond
                               (more
                                (multiple-value-bind (aliases loaded)
                                    (load-directory-aliases *directory-aliases* translation)
                                  (cond
                                   (loaded
                                    (setf *directory-aliases* aliases)
                                    (translate-and-load more))
                                   (t
                                    (warn "dangling directory alias at ~S ~
                                           (via ~{~S~^ ~}, from ~{~S~^ ~})"
                                    (first more) dirs/aliases args)
                                    nil))))
                               (t
                                (setf *directory-aliases*
                                      (load-directory-aliases *directory-aliases* translation))
                                translation)))
                             (more
                              (warn "dangling directory alias at ~S ~
                                     (via ~{~S~^ ~}, from ~{~S~^ ~})"
                                    (first more) dirs/aliases args)
                              nil)))))
                 (let ((translation (translate-and-load args)))
                   (cond
                    (translation
                     (change-directory translation)
                     (setf *last-directory* d))
                    (t
                     (setf did-cd nil))))
                 did-cd))))))
    (case cmd
      ((:cd)
       (chdir args)
       (values))
      ((:pwd)
       (format t "~&~A~%" (get-working-directory))
       (values))
      ((:pushd)
       (cond ((not (null args))
              (let ((old (get-working-directory)))
                (when (chdir args)
                  (push old *directory-stack*)
                  (show-dirstack))))
             (*directory-stack*
              (let ((here (get-working-directory)))
                (rotatef here (first *directory-stack*))
                (chdir here)
                (show-dirstack)))
             (t
              (warn "empty directory stack")))
       (values))
      ((:popd)
       (when (not (null args))
         (warn "ignoring arguments"))
       (if *directory-stack*
           (cd (pop *directory-stack*))
         (warn "empty directory stack"))
       (show-dirstack))
      ((:dirs)
       (when (> (length args) 1)
         (warn "ignoring surplus arguments"))
       (show-dirstack (first args)))
      ((:dired)
       (ed (if (null args)
               "."
             (translate-directories args)))
       (values))
      (otherwise
       (warn "unrecognised directory command ~S" cmd)
       (values)))))

(declare-extra-lw-commands
  (:cd directory-command
   "Change directory")
  (:pwd directory-command
   "Return the current working directory")
  (:pushd directory-command
   "Push a directory, or swap directories with no argument")
  (:popd directory-command
   "Pop a directory")
  (:dirs directory-command
   "Show the current working directory and stack.  With argument return them")
  (:dired directory-command
   "Edit a directory, or the current directory"))


;;;; Prompt
;;;

(defvar *prompts*
  (mapcan (lambda (pspec)
            (destructuring-bind (name . prompt) pspec
              (multiple-value-bind (result condition)
                  (ignore-errors (format nil prompt *package* 0 0))
                (cond
                 ((not result)
                  (warn "bad named prompt ~S (~A)" name condition)
                  '())
                 (t
                  (list pspec))))))
          `((:standard . ,*prompt*)
            (:short . "~%~*~D~[~:;~:* : ~D~] > ")
            (:minimal . "~%~2*~[~:;~:*~D ~]> "))))

(defvar *this-prompt* (or (cdr (assoc ':standard *prompts*)) *prompt*))
(defvar *last-prompt* (or (cdr (assoc ':minimal *prompts*)) *prompt*))

(defun prompt-command (cmd &optional arg)
  (case cmd
      ((:prompt)
     (typecase arg
      (null
       (let ((current (rassoc *prompt* *prompts* :test #'string=))
             (this (rassoc *this-prompt* *prompts* :test #'string=))
             (last (rassoc *last-prompt* *prompts* :test #'string=)))
         (format t "~&~@{~A ~S ~32,1T(~:[~*?~;~S~])~%~}"
                 '= *prompt* current (car current)
                 '+ *this-prompt* this (car this)
                 '- *last-prompt* last (car last))
         (format t "named prompts:~{~% ~S~16,1T~S~^;~}.~%"
                 (mapcan (lambda (e)
                           (list (car e) (cdr e)))
                         *prompts*))))
      (keyword
       (let ((new (assoc arg *prompts*)))
         (if new
             (setf *last-prompt* *prompt*
                   *prompt* (cdr new))
           (warn "no prompt named ~S" arg))))
      (symbol
       (let ((s (symbol-name arg)))
         (cond
          ((string= s "-")
           (psetf *prompt* *last-prompt*
                  *last-prompt* *prompt*))
          ((string= s "+")
           (psetf *prompt* *this-prompt*
                  *last-prompt* *prompt*))
          ((string= s "="))
          (t
           (warn "don't understand ~S for :prompt" arg)))))
      (string
       (multiple-value-bind (result condition)
           (ignore-errors
             (format nil arg *package* 0 0))
         (if result
             (setf *last-prompt* *prompt*
                   *prompt* arg)
           (warn "prompts need to accept three arguments, ~S seems not to (~A)"
                 arg condition))))
      (t
       (warn "don't understand ~S for :prompt" arg)))))
  (values))

(declare-extra-lw-commands
  (:prompt prompt-command
   "Control the prompt"))

(defun lrc-command (cmd &rest lrc-files)
  ;; BEWARE: *there is no WITH-STANDARD-IO-SYNTAX here*: lrc commands
  ;; get to do anything they want to.
  (case cmd
    ((:lrc)
     (let ((*read-eval* nil))
       (dolist (lrc-file lrc-files)
         (let* ((lrc-name (merge-pathnames
                           lrc-file
                           (load-time-value (make-pathname :type "lrc"))))
                (it (probe-file lrc-name)))
           (if it
               (with-open-file (lrc it)
                 (loop for form = (read lrc nil lrc)
                       until (eq form lrc)
                       do
                         (if (and (consp form)
                                  (keywordp (first form)))
                             (destructuring-bind (command &rest args) form
                               (let ((found (assoc command *extra-lw-commands*)))
                                 (if found
                                     (apply (second found) command args)
                                   (warn "no command for ~S" command))))
                           (warn "malformed command ~A" form))))
             (warn "no lrc file ~A (from ~A)" lrc-name lrc-file))))))
    (otherwise
     (warn "don't understand lrc command ~S" cmd)))
  (values))

(declare-extra-lw-commands
  (:lrc lrc-command
   "Run LW commands from an LRC file"))
