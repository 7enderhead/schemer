#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

; is parameter a list of atom?
(define (lat? l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))

; is atom 'a' a member of list of atoms 'lat'?
(define (member? a lat)
  (cond
    ((null? lat) #f)
    ((eq? a (car lat)) #t)
    (else (member? a (cdr lat)))))

; remove first occurrence (if any) of atom 'a'
; from list of atoms 'lat'
(define (rember a lat)
  (cond
    ((null? lat) '())
    ((eq? a (car lat)) (cdr lat))
    (else (cons (car lat) (rember a (cdr lat))))))

; remove all occurrences (if any) of atom 'a'
; from list of atoms 'lat'
(define (multirember a lat)
  (cond
    ((null? lat) '())
    ((eq? a (car lat)) (multirember a (cdr lat)))
    (else (cons (car lat) (multirember a (cdr lat))))))

; construct a list of the first s-expressions of
; all lists in list 'l'
(define (firsts l)
  (cond
    ((null? l) '())
    (else (cons (car (car l)) (firsts (cdr l))))))

; insert 'new' after the first occurrence of 'old'
; in list of atoms 'lat'
(define (insertR new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons old (cons new (cdr lat))))
    (else (cons (car lat) (insertR new old (cdr lat))))))

; insert 'new' after all occurrences of 'old'
; in list of atoms 'lat'
(define (multiinsertR new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
    (else (cons (car lat) (multiinsertR new old (cdr lat))))))

; insert 'new' before the first occurrence of 'old'
; in list of atoms 'lat'
(define (insertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new lat))
    (else (cons (car lat) (insertL new old (cdr lat))))))

; insert 'new' before all occurrences of 'old'
; in list of atoms 'lat'
(define (multiinsertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
    (else (cons (car lat) (multiinsertL new old (cdr lat))))))

; substitute the first occurrence of 'old' with 'new'
; in list of atoms 'lat'
(define (subst new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (cdr lat)))
    (else (cons (car lat) (subst new old (cdr lat))))))

; substitute all occurrences of 'old' with 'new'
; in list of atoms 'lat'
(define (multisubst new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
    (else (cons (car lat) (multisubst new old (cdr lat))))))

; substitute the first occurrence of either 'old1' or 'old2'
; with 'new' in list of atoms 'lat'
(define (subst2 new old1 old2 lat)
  (cond
    ((null? lat) '())
    ((or (eq? old1 (car lat)) (eq? old2 (car lat))) (cons new (cdr lat)))
    (else (cons (car lat) (subst2 new old1 old2 (cdr lat))))))

; addition based on zero?, add1 and sub1
(define (+! n m)
  (cond
    ((zero? m) n)
    (else (add1 (+! n (sub1 m))))))

; subtraction based on zero?, add1 and sub1
(define (-! n m)
  (cond
    ((zero? m) n)
    (else (sub1 (-! n (sub1 m))))))

; sum the numbers in tuple 'tup'
(define (addtup tup)
  (cond
    ((null? tup) 0)
    (else (+! (car tup) (addtup (cdr tup))))))

; multiplication based on basic operations
(define (x! n m)
  (cond
    ((zero? m) 0)
    (else (+! n (x! n (sub1 m))))))

; sum the numbers in 'tup1' and 'tup2' by their
; position, producing a new tuple containing the sums
(define (tup+ tup1 tup2)
  (cond
    ((and (null? tup1) (null? tup2)) '())
    (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))

; sum the numbers in 'tup1' and 'tup2' by their
; position, producing a new tuple containing the sums
; if one tuple is longer its additional numbers will
; be kept untouched
(define (tup++ tup1 tup2)
  (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else (cons (+ (car tup1) (car tup2)) (tup++ (cdr tup1) (cdr tup2))))))

; 'greater than' implemented with primitives
(define (>! n m)
  (cond
    ((zero? n) #f)
    ((zero? m) #t)
    (else (>! (sub1 n) (sub1 m)))))

; 'lower than' implemented with primitives
(define (<! n m)
  (cond
    ((zero? m) #f)
    ((zero? n) #t)
    (else (<! (sub1 n) (sub1 m)))))

; 'equals' for numbers based on primitives
; 'greater than' and 'lower than'
(define (=! n m)
  (cond
    ((>! n m) #f)
    ((<! n m) #f) 
    (else #t)))

; power function using primitive 'x!'
(define (^! b e)
  (cond
    ((zero? e) 1)
    (else (x! b (^! b (sub1 e))))))

; a recursive version of division
(define (/! n m)
  (cond
    ((<! n m) 0)
    (else (add1 (/! (-! n m) m)))))

; recursive version of length
(define (length! lat)
  (cond
    ((null? lat) 0)
    (else (add1 (length! (cdr lat))))))

; pick the 'n'-th element from list of
; atom 'lat'
(define (pick n lat)
  (cond
    ((=! 1 n) (car lat))
    (else (pick (sub1 n) (cdr lat)))))

; return list of atoms 'lat' without
; 'n'-th element removed
(define (rempick n lat)
  (cond
    ((=! 1 n) (cdr lat))
    (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

; remove all numbers from list of atoms 'lat'
(define (no-nums lat)
  (cond
    ((null? lat) '())
    (else (cond
            ((number? (car lat)) (no-nums (cdr lat)))
            (else (cons (car lat) (no-nums (cdr lat))))))))

; extract all numbers from list of atoms 'lat'
(define (all-nums lat)
  (cond
    ((null? lat) '())
    (else (cond
            ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
            (else (all-nums (cdr lat)))))))

; are atoms equal, either compared as numbers via '='
; or with 'eq?' for all other atoms
(define (eqan? a1 a2)
  (cond
    ((and (number? a1) (number? a2)) (=! a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2))))

; How many times does 'a' occur in 'lat'?
(define (occur a lat)
  (cond
    ((null? lat) 0)
    (else (cond
            ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
            (else (occur a (cdr lat)))))))

; Is the given number 1?
(define (one? n)
  (=! 1 n))

; remove all occurences of atom 'a' from
; list 'l', including from nested lists
(define (rember* a l)
  (cond
    ((null? l) '())
    ((and (atom? (car l)) (eqan? a (car l))) (rember* a (cdr l)))
    ((atom? (car l)) (cons (car l) (rember* a (cdr l))))
    (else (cons (rember* a (car l)) (rember* a (cdr l))))))

; insert 'new' after each occurrence of 'old'
; in list l, even in nested lists
(define (insertR* new old l)
  (cond
    ((null? l) '())
    ((and (atom? (car l)) (eqan? old (car l))) (cons old (cons new (insertR* new old (cdr l)))))
    ((atom? (car l)) (cons (car l) (insertR* new old (cdr l))))
    (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))

; number of occurrences of atom 'a' in list 'l',
; including nested lists
(define (occur* a l)
  (cond
    ((null? l) 0)
    ((and (atom? (car l)) (eqan? a (car l))) (add1 (occur* a (cdr l))))
    ((atom? (car l)) (occur* a (cdr l)))
    (else (+! (occur* a (car l)) (occur* a (cdr l))))))