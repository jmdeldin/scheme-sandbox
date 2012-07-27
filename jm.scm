;;; jm.scm -- various utility functions
;;

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

;; This uses the basic constructs we've created thus far, but this could
;; simply be
;;   (lambda (x y) (not (eq? x y)))
;; if your interpreter has a `not` function defined.
(define neq?
  (lambda (x y)
    (cond
     ((eq? x y) #f)
     (else #t))))


;; (excludes? 'atom '(a list)) ;; => #t
;; (excludes? 'atom '(atom))   ;; => #f
(define excludes?
  (lambda (a lat)
    (cond
     ((null? lat) #t)
     ((eq? a (car lat)) #f)
     (else (excludes? a (cdr lat))))))

;; Apply a function to every list element. 'eval is needed to turn 'square
;; into its definition; otherwise, only inline lambdas would be allowed.
(define (jm/map f l)
  (cond
   ((null? l) l)
   (else
    (cons ((eval f) (car l)) (jm/map f (cdr l))))))

;; (reduce 'plus '(1 2 3)) ;; => 6
(define (jm/reduce f l)
  (define (aux l acc)
    (cond
     ((null? l) acc)
     (else
      (aux (cdr l) ((eval f) acc (car l))))))
  (aux l 0))

;; tail-call optimized sum
(define (jm/sum l)
  (jm/reduce (lambda (x y) (+ x y)) l))

;; exclusive range = 0...3
;; (jm/range 0 3) => '(0 1 2)
(define (jm/xrange low high)
  (cond
   ((eq? low high) '())
   (else
    (cons low (jm/xrange (+ low 1) high)))))

;; inclusive range = 0..3
;; (jm/range 0 3) => '(0 1 2 3)
(define (jm/range low high)
  (cond
   ((> low high) '())
   (else
    (cons low (jm/range (+ low 1) high)))))

(define (jm/length l)
  (define (aux ctr lst)
    (cond
     ((null? lst) ctr)
     (else
      (aux (+ 1 ctr) (cdr lst)))))
  (aux 0 l))

;; (jm/select (lambda (x) (eq? x 2)) '(2 3 4 2)) => '(2 2)
(define (jm/select f l)
  (define (aux matches remaining)
    (if (null? remaining)
        matches
        (aux (if ((eval f) (car remaining))
                 (cons (car remaining) matches)
                 matches) (cdr remaining))))
  (aux '() l))

(define (jm/negate f)
  (lambda (x)
    (not ((eval f) x))))

;; (jm/reject (lambda (x) (< x 3)) '(1 2 3 4)) => '(3 4)
(define (jm/reject f l)
  (jm/select (jm/negate f) l))

;;; Silly functions
(define square
  (lambda (x)
    (* x x)))

;; partial application of plus
(define (plus x)
  (lambda (y)
    (+ x y)))
