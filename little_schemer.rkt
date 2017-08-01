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