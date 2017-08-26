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

; substitute all occurrences of 'old' with 'new' in list 'l',
; including nested lists
(define (subst* new old l)
  (cond
    ((null? l) '())
    ((and (atom? (car l)) (eqan? old (car l))) (cons new (subst* new old (cdr l))))
    ((atom? (car l)) (cons (car l) (subst* new old (cdr l))))
    (else (cons (subst* new old (car l)) (subst* new old (cdr l))))))

; insert 'new' before each occurrence of 'old'
; in list l, even in nested lists
(define (insertL* new old l)
  (cond
    ((null? l) '())
    ((and (atom? (car l)) (eqan? old (car l))) (cons new (cons old (insertL* new old (cdr l)))))
    ((atom? (car l)) (cons (car l) (insertL* new old (cdr l))))
    (else (cons (insertL* new old (car l)) (insertL* new old (cdr l))))))

; is atom 'a' a member of list 'l',
; including nested lists
(define (member* a l)
  (cond
    ((null? l) #f)
    ((atom? (car l)) (or (eqan? a (car l)) (member* a (cdr l))))
    (else (or (member* a (car l)) (member* a (cdr l))))))

; Are the two lists equal?
(define (eqlist? l1 l2)
  (cond
    ; two empty lists are equal
    ((and (null? l1) (null? l2)) #t)
    ; only one list is empty
    ((or (null? l1) (null? l2)) #f)
    ; both first elements are atoms and can be compared, along with the rest of the list
    ((and (atom? (car l1)) (atom? (car l2)))
     (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
    ; both first elements are list and can be further compared, along with the rest of the list
    ((and (list? (car l1)) (list? (car l2)))
     (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
    (else #f)))

; equality for atoms and S-expressions
(define (equal?! s1 s2)
  (cond
    ((and (atom? s1) (atom? s2) (eqan? s1 s2)))
    ((or (atom? s1) (atom? s2)) #f)
    (else (eqlist?! s1 s2))))

; eqlist version which uses equal??
(define (eqlist?! l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    (else (and (equal?! (car l1) (car l2)) (eqlist?! (cdr l1) (cdr l2))))))

; Does the representation of an arithmentic expression 'aexp'
; only contain numbers besides '+', 'x', '^'?
(define (numbered? aexp)
  (cond
    ; arithmetic expression that are atoms should be numbers
    ((atom? aexp) (number? aexp))
    (else (and
           (numbered? (car aexp))
           (or (eq? '+ (cadr aexp)) (eq? 'x (cadr aexp)) (eq? '^ (cadr aexp)))
           (numbered? (caddr aexp))))))

; value of given in-fix arithmetic expression
(define (in-value exp)
  (cond
    ((atom? exp) exp)
    ((eq? '+ (cadr exp)) (+ (in-value (car exp)) (in-value (caddr exp))))
    ((eq? 'x (cadr exp)) (* (in-value (car exp)) (in-value (caddr exp))))
    ((eq? '^ (cadr exp)) (expt (in-value (car exp)) (in-value (caddr exp))))))

; value of given pre-fix arithmetic expression
(define (pre-value exp)
  (cond
    ((atom? exp) exp)
    ((eq? '+ (car exp)) (+ (pre-value (cadr exp)) (pre-value (caddr exp))))
    ((eq? 'x (car exp)) (* (pre-value (cadr exp)) (pre-value (caddr exp))))
    ((eq? '^ (car exp)) (expt (pre-value (cadr exp)) (pre-value (caddr exp))))))

; functions for numbers represented as lists:
; (): 0
; (()): 1
; (() ()): 2, etc

(define (sero? n)
  (eq? '() n))

(define (edd1 n)
  (cons '() n))

(define (zub1 n)
  (cdr n))

(define (+!! n m)
  (cond
    ((sero? m) n)
    (else (+!! (edd1 n) (zub1 m)))))

; Is list of atoms 'lat' a set, i.e., no atom appears
; more than once?
(define (set? lat)
  (cond
    ((null? lat) #t)
    (else (and (not (member? (car lat) (cdr lat)))
               (set? (cdr lat))))))

; produce a set from list of atoms 'lat' by removing
; duplicate entries
(define (makeset lat)
  (cond
    ((null? lat) '())
    (else (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))))

; Is 'set1' a subset of 'set2'?
(define (subset? set1 set2)
  (cond
    ((null? set1) #t)
    (else (and (member? (car set1) set2)
               (subset? (cdr set1) set2)))))

; Are 'set1' and 'set2' equal, i.e., do they contain
; the same elements?
(define (eqset? set1 set2)
  (and (subset? set1 set2) (subset? set2 set1)))

; Do 'set1' and 'set2' have any elements in common?
(define (intersect? set1 set2)
  (cond
    ((null? set1) #f)
    (else (or (member? (car set1) set2)
              (intersect? (cdr set1) set2)))))

; Elements which 'set1' and 'set2' have in common
(define (intersect set1 set2)
  (cond
    ((null? set1) '())
    ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
    (else (intersect (cdr set1) set2))))

; Elements which are either in 'set1' or 'set2' (or both)
(define (union set1 set2)
  (cond
    ((null? set1) set2)
    ((member? (car set1) set2) (union (cdr set1) set2))
    (else (cons (car set1) (union (cdr set1) set2)))))

; All elements of 'set1' that are not in 'set2'
(define (difference set1 set2)
  (cond
    ((null? set1) '())
    ((member? (car set1) set2) (difference (cdr set1) set2))
    (else (cons (car set1) (difference (cdr set1) set2)))))

; Is atom 'a' a member of all lists in list of lists 'lists'?
(define (memberall? a lists)
  (cond
    ((null? lists) #t)
    (else (and (member? a (car lists)) (memberall? a (cdr lists))))))

; Intersection of all sets in (non-empty) list of sets 'l-set'
(define (intersectall!! l-set)
  (cond
    ((null? (car l-set)) '())
    ((memberall? (caar l-set) (cdr l-set)) (cons (caar l-set) (intersectall (cons (cdar l-set) (cdr l-set)))))
    (else (intersectall (cons (cdar l-set) (cdr l-set))))))

; And here the much easier version from the book
(define (intersectall l-set)
  (cond
    ((null? (cdr l-set)) (car l-set))
    (else (intersect (car l-set) (intersectall (cdr l-set))))))