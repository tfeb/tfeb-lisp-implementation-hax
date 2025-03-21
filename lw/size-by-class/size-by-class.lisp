;;;; LW size by class
;;;

#-LispWorks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "Not LW"))

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 ((:org.tfeb.hax.collecting
   :org.tfeb.star)
  :compile t))

(defpackage :org.tfeb.lw.size-by-class
  (:use :cl)
  (:use :hcl)
  (:use :org.tfeb.hax.collecting :org.tfeb.star)
  (:export
   #:size-by-class
   #:log-size-by-class
   #:class-counter #:class-counter-name #:class-counter-count
   #:class-counter-total-size #:class-counter-cumulative-size))

(in-package :org.tfeb.lw.size-by-class)

(provide :org.tfeb.lw.size-by-class)

(defstruct class-counter
  name
  (count 0)
  (total-size 0)
  (cumulative-size 0))

(defun size-by-class (&key (gen-0 nil) (min-ratio nil) (test nil) (renormalize nil))
  (let ((stab (make-hash-table)))
    (sweep-all-objects
     (lambda (o)
       (when (if test (funcall test o) t)
         (let* ((c (class-name (class-of o)))
                (cc (or (gethash c stab)
                        (setf (gethash c stab) (make-class-counter :name c)))))
           (incf (class-counter-total-size cc) (find-object-size o))
           (incf (class-counter-count cc)))))
     gen-0)
    (let ((cumulative
           (nreverse
            (collecting
              (for (((cc a)
                     (stepping*
                      (cct :initially (sort (collecting
                                              (for (((_ cc) (in-hash-table stab)))
                                                (collect cc)))
                                            #'<
                                            :key #'class-counter-total-size)
                           :then (rest cct) :until (null cct) :value nil)
                      (cc :as (first cct))
                      (a :initially (if cc (class-counter-total-size cc) 0)
                         :then (+ a (if cc (class-counter-total-size cc) 0))))))
                (setf (class-counter-cumulative-size cc) a)
                (collect cc))))))
      (if (and min-ratio (not (null cumulative)))
          (let* ((cumulative-size (class-counter-cumulative-size (first cumulative)))
                 (ccs (ldiff cumulative (member-if (lambda (size)
                                                     (< (/ size cumulative-size) min-ratio))
                                                   cumulative
                                                   :key #'class-counter-cumulative-size))))
            (if renormalize
                (let ((reduction (- cumulative-size (reduce #'+ ccs
                                                            :key #'class-counter-total-size))))
                  (for ((cc (in-list ccs)))
                    (decf (class-counter-cumulative-size cc) reduction)))
              (let* ((last-cons (last ccs))
                     (last-cc (car last-cons)))
                (let ((size (- (class-counter-cumulative-size last-cc)
                               (class-counter-total-size last-cc))))
                  (setf (cdr last-cons)
                        (list (make-class-counter :name nil
                                                  :count 1
                                                  :total-size size
                                                  :cumulative-size size))))))
            ccs)
        cumulative))))

(defun dump-cumulative-sizes (sizes to &key (append nil) (header (not append)))
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-pretty* t))
      (with-open-file (out to :direction :output :if-exists (if append ':append ':supersede))
        (when header
          (format out "~&;;; (ut (name count total-size cumulative-size) ...)~%;;;"))
        (print
         (collecting
           (collect (get-universal-time))
           (for ((cc (in-list sizes)))
             (collect (list (class-counter-name cc)
                            (class-counter-count cc)
                            (class-counter-total-size cc)
                            (class-counter-cumulative-size cc)))))
         out))))
  to)

(defun log-size-by-class (to &key (every 60) (count nil) (append nil)
                             (min-ratio 0.05) (renormalize nil))
  (for ((iteration (in-naturals))
        (first (sequentially* t nil)))
    (when (and count (>= iteration count)) (final iteration))
    (dump-cumulative-sizes (size-by-class :min-ratio min-ratio :renormalize renormalize)
                           to
                           :append (or append (not first)))
    (when (and count (>= iteration (1- count))) (final (1+ iteration)))
    (sleep every)))
