#lang racket/base

(define version "1.4")

; Spectral-norm Benchmark - Generates a real-time measure of multi-core performance.
; by Dexter Santucci
; Translated from Mike Pall's Lua version.
; Parallelized by Sam Tobin-Hochstadt.
; http://benchmarksgame.alioth.debian.org/
;
; Version history :
; v1.4 - fixed core and hyper-threading count, updated compiler to Racket 8.10.
; v1.3 - added support for hyper-threading.
; v1.2 - compiled with Racket 7.1.
; v1.1 - added burn-in mode.
; v1.0 - initial release.

;;; defs
(require racket/future racket/require (for-syntax racket/base)
         (filtered-in (λ (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops)
         (only-in racket/flonum make-flvector))

;; parallel for
(define-syntax-rule (for/par k ([i N]) b)  
  (let ([stride (fxquotient N k)])
    (define fs 
      (for/list ([n k])
        (future (λ () (for ([i (in-range (fx* n stride) (fxmin N (fx* (fx+ n 1) stride)))]) b)))))
    (for-each touch fs)))

;; the big let improves performance by about 20% - minimum is 8, benchmark fixed at 5500
(define (spectral-norm number max-cores)
  (let* ()
    (define N number)
    (define C max-cores)
    (define (A i j)
      (let ([ij (fx+ i j)])
        (fl/ 1.0 (fl+ (fl* (fl* (fx->fl ij)
                                (fx->fl (fx+ ij 1)))
                           0.5) 
                      (fx->fl (fx+ i 1))))))
    (define (Av x y N)
      (for/par C ([i N])
        (flvector-set!
         y i
         (let L ([a 0.0] [j 0])
           (if (fx= j N) a
               (L (fl+ a (fl* (flvector-ref x j) (A i j)))
                  (fx+ j 1)))))))
    (define (Atv x y N)
      (for/par C ([i N])
        (flvector-set!
         y i
         (let L ([a 0.0] [j 0])
           (if (fx= j N) a
               (L (fl+ a (fl* (flvector-ref x j) (A j i)))
                  (fx+ j 1)))))))
    (define (AtAv x y t N) (Av x t N) (Atv t y N))
    (define u (make-flvector N 1.0))
    (define v (make-flvector N))
    (define t (make-flvector N))
    (for ([i (in-range 10)])
      (AtAv u v t N) (AtAv v u t N))
    ;    (displayln (real->decimal-string                                            ; uncomment to display result
    ;                (flsqrt 
    ;                 (let L ([vBv 0.0] [vv 0.0] [i 0])
    ;                   (if (fx= i N) (fl/ vBv vv)
    ;                       (let ([ui (flvector-ref u i)] [vi (flvector-ref v i)])
    ;                         (L (fl+ vBv (fl* ui vi))
    ;                            (fl+ vv (fl* vi vi))
    ;                            (fx+ i 1))))))
    ;                9))
    ))

;; define a new namespace in the current benchmark module
(define-namespace-anchor benchmark-anchor)

;; repeatedly evaluate thunk for one second
(define (do-for-1s thunk)
  (define bench-ns (namespace-anchor->namespace benchmark-anchor))
  (do ((start-time (current-inexact-milliseconds))
       (result #f (eval thunk bench-ns)))
    ((> (- (current-inexact-milliseconds) start-time) 1000))))

;; repeatedly evaluate thunk for X second and return the number of times it was evaluated
(define (how-many-in seconds thunk)
  (when (> seconds 0)
    (define bench-ns (namespace-anchor->namespace benchmark-anchor))
    (do ((count 0 (+ 1 count))
         (start-time (current-inexact-milliseconds))
         (result #f (eval thunk bench-ns)))
      ((> (- (current-inexact-milliseconds) start-time) (* seconds 1000)) count))))

;; one test + "." display
(define (test max-cores)
  (how-many-in 0.25 `(begin (display ".")
                            (spectral-norm 170 ,max-cores))))

;; initialize a list of results
(define results null)

;; calculates the average of a list
(define (average l)
  (if (> (length l) 0) (exact->inexact (/ (apply + l) (length l)))
      0))

;; main benchmark display loop - interrupt with ctrl-c
(define (test-batch count max-cores)
  (let* ((result (test max-cores)))
    (begin (set! results (cons result results))
           (displayln result)
           result)
    (unless (= count 1) (test-batch (- count 1) max-cores))))

;; generic pause
(define (pause)
  (display "\nPress [Enter] to continue or [Ctrl-C] to abort.\n")
  (void (read-line)))

;; display info page
(define (info)
  (displayln (string-append "Spectral-norm Benchmark v" version " - Generates a real-time measure of multi-core performance.\n"
                            "by Dexter Santucci\n"
                            "Translated from Mike Pall's Lua version.\n"
                            "Parallelized by Sam Tobin-Hochstadt.\n"
                            "http://benchmarksgame.alioth.debian.org/\n"
                            "Reference: Intel i7-4790 = 100 multi-threaded.")))

;;; main

(info)
(pause)

(displayln "Running single-thread test...")
(test-batch 10 1)
(display "Average: ") (displayln (average results))
(set! results null) ; reset results

(displayln "\nRunning multi-core test (using all cores)...")
(test-batch 10 (/ (processor-count) 2))
(display "Average: ") (displayln (average results))

(displayln "\nRunning hyper-threaded test (using all cores * 2)...")
(test-batch 10 (processor-count))
(display "Average: ") (displayln (average results))

(displayln "\nReady to run in burn-in mode for 50,000 iterations.")
(pause)

(displayln "\nRunning multi-threaded test (using all cores)...")
(test-batch 50000 (processor-count))
(display "Average: ") (displayln (average results))


; EOF
