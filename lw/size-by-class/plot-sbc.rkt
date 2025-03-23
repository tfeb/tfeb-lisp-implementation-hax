#!/usr/bin/env racket
#lang racket

(module plot-sbc-dump-file racket
  ;;; Guts of the plotting program
  ;;;

  (require
   plot
   (only-in plot/utils
            color-seq))

  (provide
   plot-dump-file-to)

  (plot-font-family 'modern)
  (plot-width 560)
  (plot-legend-anchor 'top-left)
  (plot-x-far-axis? #f)
  (plot-y-far-axis? #f)

  (struct class-counter
    (name
     count
     total-size
     cumulative-size)
    #:transparent)

  ;;; Each entry in a dump is (ut (name count total-size
  ;;; cumulative-size) ...)  If name is nil it's the remaining memory.
  ;;;

  (define (snarf-dump from (transformer class-counter))
    ;; Based on warranted (wcs.rkt)
    (call-with-default-reading-parameterization
     (thunk
      (parameterize ([read-accept-lang #f]
                     [read-accept-reader #f])
        (call-with-input-file from
          (lambda (port)
            (for/list ([form (in-port read port)])
              (if transformer
                  (cons (car form)
                        (map (λ (e)
                               (apply transformer e))
                             (rest form)))
                  form))))))))

  (define (biggest-classes dump n)
    ;; Return a set of up to n classes with the largest peak
    ;; allocation in the dump, and the largest total allocation.  If n
    ;; is true but not a number return them all
    (define class-size-table (make-hasheqv))
    (define total-allocation
      (for/fold ([total-allocation 0])
                ([entry (in-list dump)])
        (match entry
          [(cons _ records)
           (define this-total
             (for/sum ([record (in-list records)])
               (define name (class-counter-name record))
               (define total-size (class-counter-total-size record))
               (hash-update! class-size-table name
                             (λ (v) (max v total-size))
                             0)
               total-size))
           (max total-allocation this-total)])))
    (define sorted (sort (for/list ([(name value) (in-hash class-size-table)])
                           (cons name value))
                         > #:key cdr))
    (if (not (null? sorted))
        (values
         (cond
           [(number? n)
            (for/seteqv ([e (in-list sorted)]
                         [_ (in-range n)])
              (car e))]
           [n
            (for/seteqv ([e (in-list sorted)])
              (car e))]
           [else
            (seteqv)])
         (cdr (first sorted))
         total-allocation)
        (values (seteqv) 0 0)))

  (define (transform-dump dump #:classes (classes #t) #:total (total #f))
    ;; Transform a dump into something plottable.
    (define sequences (make-hasheqv))
    (define start-ut (car (first dump)))
    (for ([entry (in-list dump)])
      (match entry
        [(cons ut records)
         (define rt (- ut start-ut))
         (define the-total
           (for/sum ([record (in-list records)])
             (define name (class-counter-name record))
             (define total-size (class-counter-total-size record))
             (when (or (eqv? classes #t)
                       (set-member? classes name))
               (hash-update! sequences name
                             (λ (v)
                               (cons (list rt total-size)
                                     v))
                             '()))
             total-size))
         (when total
           (hash-update! sequences 't
                         (λ (v)
                           (cons (list rt the-total)
                                 v))
                         '()))]))
    (for/list ([(name entries) (in-hash sequences)])
      (cons name (reverse entries))))

  (define (plot-dump-file-to dump-file to
                             #:title (title "LW memory usage by class")
                             #:n (n #t)
                             #:total (total #f))
    (define dump (with-handlers
                   ([exn:fail:read?
                     (λ (e)
                       (sleep 1)
                       (snarf-dump dump-file))])
                   (snarf-dump dump-file)))
    (let-values ([(classes largest total-allocation) (biggest-classes dump n)])
      (define td (transform-dump dump #:classes classes #:total total))
      (plot-file
       (for/list ([r (in-list td)]
                  [c (in-list (color-seq "blue" "yellow"
                                         (+ (length td)
                                            (if total 1 0))))])
         (match r
           [(cons 'nil entries)
            (lines entries #:color c #:label "(small)")]
           [(cons 't entries)
            (lines entries #:color c #:label "(total)")]
           [(cons name entries)
            (lines entries #:color c #:label (symbol->string name))]))
       to
       #:y-label "size (bytes)"
       #:x-label "time (seconds)"
       #:y-min 0
       #:y-max (* (if total total-allocation largest) 1.05)
       #:title (if (number? n)
                   (format "~A (~S biggest)" title n)
                   title))))
  )                                     ;end of plot-sbc

(module main racket
  ;;; Plot size-by-class dumps, driver
  ;;;

  (require
   (submod ".." plot-sbc-dump-file)
   (only-in racket/cmdline
            command-line))

  (define me "plot-sbc")

  (define number-of-classes (make-parameter #t))
  (define plot-total-as-well (make-parameter #f))

  (struct exn:fail:death exn:fail ()
    #:extra-constructor-name make-exn:fail:death
    #:transparent)

  (define (die fmt . args)
    (raise (make-exn:fail:death
            (apply format (format "~A: ~A" me fmt) args)
            (current-continuation-marks))))

  (define (complain format . args)
    (apply fprintf (current-error-port) format args))

  (with-handlers ([exn:fail:death?
                   (λ (e)
                     (complain "~A~%" (exn-message e))
                     (exit 1))]
                  [exn:fail:filesystem?
                   (λ (e)
                     (complain "~A~%" (exn-message e))
                     (exit 1))]
                  [exn?
                   (λ (e)
                     (complain "mutant death: ~A~%"
                              (exn-message e)))])
    (command-line
     #:program me
     #:once-each
     (("-n" "--number")
      number
      "number of classes to plot"
      (define n (string->number number))
      (cond
        [(exact-nonnegative-integer? n)
          (number-of-classes n)]
        [(number? n)
         (die "~A isn't a nonnegative integer" n)]
        [else
         (die "~S isnt a number at all" number)]))
     (("-t" "--total")
      "also plot the total"
      (plot-total-as-well #t))
     #:handlers
     (λ (_ . args)
       (match args
         [(list from to)
          (plot-dump-file-to from to
                             #:n (number-of-classes)
                             #:total (plot-total-as-well))
          (exit 0)]
         [else
          (die "need dump file, image file")]))
     '("dump-file" "image-file")
     (λ (help-message)
       (complain "~A~%" help-message)
       (exit 0))
     (λ switches
       (case (length switches)
         [(1)
          (die "unknown switch ~A (try -h)" (first switches))]
         [(0)
          (die "mutant switch death")]
         [else
          (die "unknown switches ~A (try -h)" switches)]))))
  )                                     ;end of main
