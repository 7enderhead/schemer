#lang racket

(define (display-all . vs)
  (for-each display vs))

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

; Is 'x' a pair, i.e., a list with exactly two s-expressions?
(define (a-pair? x)
  (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cddr x)) #t)
    (else #f)))

; remove the first element from list 'l' which
; matches 'a' given test function 'test?'
(define (rember-f_ test? a l)
  (cond
    ((null? l) '())
    ((test? a (car l)) (cdr l))
    (else (cons (car l) (rember-f_ test? a (cdr l))))))

; return a function which compares a given parameter 'x' to 'a'
; using eq?
(define (eq?-c a)
  (lambda (x)
    (eq? x a)))

; version of rember-f whose result is a function with two arguments
; 'a' und 'l', which, when invoked, removes the first match of 'a'
; with 'test?' from 'l'
(define (rember-f test?)
  (lambda (a l)
    (cond
    ((null? l) '())
    ((test? a (car l)) (cdr l))
    (else (cons (car l) ((rember-f test?) a (cdr l)))))))

; parameterizable version of insertL where the test function 'test?'
; is given and the result is a function with three parameters 'new', 'old' and 'l' which
; inserts 'new' after the first occurrence of 'old' in 'l' which is matched by 'test?'
(define (insertL-f test?)
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((test? old (car l)) (cons new (cons old (cdr l))))
      (else (cons (car l) ((insertL-f test?) new old (cdr l)))))))

; insertR as above
(define (insertR-f test?)
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((test? old (car l)) (cons old (cons new (cdr l))))
      (else (cons (car l) ((insertR-f test?) new old (cdr l)))))))


; a generalization of insertL-f and insertR-f with the insert part factored out
(define (insert-g seq)
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? old (car l)) (seq new old (cdr l)))
      (else (cons (car l) ((insert-g seq) new old (cdr l)))))))

; helper function for insert-g which makes it act like 'subst'
(define (seq-subst new old l)
  (cons new l))

; return the appropriate function (+, *, expt) according
; to which symbol is given ('+', '*', or '^')
(define (atom-to-function x)
  (cond
    ((eq? '+ x) +)
    ((eq? '* x) *)
    ((eq? '^ x) expt)))

; access helper functions for prefix expressions

(define (1st-sub-exp nexp)
  (cadr nexp))

(define (2nd-sub-exp nexp)
  (caddr nexp))

(define (operator nexp)
  (car nexp))

; generic version of 'value' of arithmetic expressions
(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    (((atom-to-function (operator nexp))
      (value (1st-sub-exp nexp))
      (value (2nd-sub-exp nexp))))))

; a version of multirember (removing all occurrences of an atom from a list of atoms)
; parameterized with a 'test?' function, which tells us whether an atom should be remove
(define (multiremberT test? lat)
  (cond
    ((null? lat) '())
    ((test? (car lat)) (multiremberT test? (cdr lat)))
    (else (cons (car lat) (multiremberT test? (cdr lat))))))

; Insert 'new' to the left of all 'oldL' and to the right of all
; 'oldR' in list of atoms 'lat'
(define (multiinsertLR new oldL oldR lat)
  (cond
    ((null? lat) '())
    ((eq? oldL (car lat)) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
    ((eq? oldR (car lat)) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
    (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat))))))

; Version of multiinserLR with collector 'col' which is called with the
; new list with substitutions, and the number of left and right insertions,
; respectively.
(define (multiinsertLR&co new oldL oldR lat col)
  (cond
    ((null? lat) (col '() 0 0))
    ((eq? oldL (car lat)) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat leftInserts rightInserts)
                                                                      (col (cons new (cons oldL newlat)) (add1 leftInserts) rightInserts))))
    ((eq? oldR (car lat)) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat leftInserts rightInserts)
                                                                      (col (cons oldR (cons new newlat)) leftInserts (add1 rightInserts)))))
    (else (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat leftInserts rightInserts)
                                                      (col (cons (car lat) newlat) leftInserts rightInserts))))))

; version of above with intermediate output displayed
(define (multiinsertLR&co2 new oldL oldR lat col)
  (cond
    ((null? lat) (display-all "initial:\t" col " '() 0 0\n") (col '() 0 0))
    ((eq? oldL (car lat)) (multiinsertLR&co2 new oldL oldR (cdr lat) (lambda (newlat leftInserts rightInserts)
                                                                       (display-all "oldL match:\t" newlat " " leftInserts " " rightInserts " -> " col " " (cons new (cons oldL newlat)) " " (add1 leftInserts) " " rightInserts "\n")
                                                                      (col (cons new (cons oldL newlat)) (add1 leftInserts) rightInserts))))
    ((eq? oldR (car lat)) (multiinsertLR&co2 new oldL oldR (cdr lat) (lambda (newlat leftInserts rightInserts)
                                                                       (display-all "oldR match:\t" newlat " " leftInserts " " rightInserts " -> " col " " (cons oldR (cons new newlat)) " " leftInserts " " (add1 rightInserts) "\n")
                                                                      (col (cons oldR (cons new newlat)) leftInserts (add1 rightInserts)))))
    (else (multiinsertLR&co2 new oldL oldR (cdr lat) (lambda (newlat leftInserts rightInserts)
                                                       (display-all "no match:\t" newlat " " leftInserts " " rightInserts " -> " col " " (cons (car lat) newlat) " " leftInserts " " rightInserts "\n")
                                                      (col (cons (car lat) newlat) leftInserts rightInserts))))))

; Removes all odd numbers from list of nested lists 'l'
(define (evens-only* l)
  (cond
    ((null? l) '())
    ((and (atom? (car l)) (even? (car l))) (cons (car l) (evens-only* (cdr l))))
    ((atom? (car l)) (evens-only* (cdr l)))
    (else (cons (evens-only* (car l)) (evens-only* (cdr l))))))

; Collector version of evens-only* that also calculates the product of even and
; the sum of odd numbers.
(define (evens-only*&co l col)
  (cond
    ((null? l) (col '() 1 0))
    ((and (atom? (car l)) (even? (car l)))
     (evens-only*&co (cdr l) (lambda (newList evenProduct oddSum)
                               (col (cons (car l) newList) (* (car l) evenProduct) oddSum))))
    ((atom? (car l))
     (evens-only*&co (cdr l) (lambda (newList evenProduct oddSum)
                               (col newList evenProduct (+ (car l) oddSum)))))
    (else (evens-only*&co (car l) (lambda (carList carEvenProduct carOddSum)
                                    (evens-only*&co (cdr l) (lambda (cdrList cdrEvenProduct cdrOddSum)
                                                              (col (cons carList cdrList)
                                                                   (* carEvenProduct cdrEvenProduct)
                                                                   (+ carOddSum cdrOddSum)))))))))

; Look for atom 'a' in list of atoms 'lat' by starting at first position;
; following to the element at the number given there or comparing against
; 'a' if it is a symbol.
(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))

; Keep looking for atom 'a' which symbol or number 'sorn' in lat:
; Either continue with element at position 'sorn' if it is a number or
; compare it to 'a'.
(define (keep-looking a sorn lat)
  (cond
    ((number? sorn) (keep-looking a (pick sorn lat) lat))
    (else (eq? sorn a))))

(define (build sl s2)
  (cons sl (cons s2 '())))

; Take a pair whose first component is a pair and build a
; pair by shifting the second part oft the first component
; into the second component.
(define (shift pair)
  (build (first (first pair))
         (build (second (first pair))
                (second pair))))

; Align the given pair or atom 'pora'.
(define (align pora)
  (cond
    ((atom? pora) pora)
    ((a-pair? (first pora)) (align (shift pora)))
    (else (build (first pora) (align (second pora))))))

; Count the atoms in the (potentially nested) pair(s).
(define (length* pora)
  (cond
    ((atom? pora) 1)
    (else (+ (length* (first pora))
             (length* (second pora))))))

; 'Weigh' the given nested pairs by assigning twice as much weight to
; the first element than to the second.
(define (weight* pora)
  (cond
    ((atom? pora) 1)
    (else (+ (* 2 (weight* (first pora)))
             (weight* (second pora))))))

(define (Collatz n)
  (cond
    ((one? n) 1)
    (else
     (cond
       ((even? n) (Collatz (/ n 2)))
       (else (Collatz (add1 (* 3 n))))))))

(define A-invocations 0)

(define (A a b)
  (set! A-invocations 0)
  (Ackermann a b))

(define (Ackermann a b)
  (set! A-invocations (add1 A-invocations))
  (display-all A-invocations ":\t" a "\t" b "\n")
  (cond
    ((> A-invocations 10000) (error "exceeded max. invocations"))
    ((zero? a) (add1 b))
    ((zero? b) (Ackermann (sub1 a) 1))
    (else (Ackermann (sub1 a) (Ackermann a (sub1 b))))))

(define (eternity x)
  (eternity x))

; length-0
#;((lambda (next-length) (lambda (l)
                           (cond
                             ((null? l) 0)
                             (else (add1 next-length (cdr l))))))
   eternity)

; length-1
#;((lambda (next-length) (lambda (l)
                           (cond
                             ((null? l) 0)
                             (else (add1 (next-length (cdr l)))))))
   ((lambda (next-length) (lambda (l)
                            (cond
                              ((null? l) 0)
                              (else (add1 (next-length (cdr l)))))))
    eternity))

; length-0
#;((lambda (mk-length)
   (mk-length eternity)) (lambda (next-length)
                           (lambda (l)
                             (cond
                               ((null? l) 0)
                               (else (add1 (next-length (cdr l))))))))
; length-1
#;((lambda (mk-length)
     (mk-length
      (mk-length eternity))) (lambda (next-length)
                               (lambda (l)
                                 (cond
                                   ((null? l) 0)
                                   (else (add1 (next-length (cdr l))))))))

; length-n; this now works
#;((lambda (mk-length)
     (mk-length mk-length)) (lambda (mk-length)
                                (lambda (l)
                                  (cond
                                    ((null? l) 0)
                                    (else (add1 ((mk-length mk-length) (cdr l))))))))

; But in the inner (add1 (mk-length mk-length) (cdr l)) we want to write
; instead (add1 (length (cdr l)), so...

; eta-conversion of inner (mk-length mk-length) expression
#;((lambda (mk-length)
     (mk-length mk-length)) (lambda (mk-length)
                              (lambda (l)
                                (cond
                                  ((null? l) 0)
                                  (else (add1 ((lambda (x)
                                                 ((mk-length mk-length) x))
                                               (cdr l))))))))

; pull eta-conversion out; now can be referenced by parameter name 'length' without
; being evaluated prematurely when being passed as parameter
#;((lambda (mk-length)
     (mk-length mk-length)) (lambda (mk-length)
                              ((lambda (length)
                                 (lambda (l)
                                   (cond
                                     ((null? l) 0)
                                     (else (add1 (length (cdr l)))))))
                               (lambda (x)
                                 ((mk-length mk-length) x)))))

; now factor out the (lambda (length) ...) part, which is independent from
; the rest
#;((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length)) (lambda (mk-length)
                               (le (lambda (x)
                                     ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

; the Y combinator
(define (Y function)
    ((lambda (f) (f f))
     (lambda (f)
       (function (lambda (x) ((f f) x))))))

; Y combinator for function with two arguments
(define (Y2 function)
    ((lambda (f) (f f))
     (lambda (f)
       (function (lambda (x y) ((f f) x y))))))

#;((Y2 (lambda (length)
   (lambda (l x)
     (cond
       ((or (null? l) (zero? x)) 0)
       (else (add1 (length (cdr l) (sub1 x))))))))
   '(a b c) 5)

; Create a new entry. An entry is a pair of lists whose first
; list must be a set.
(define new-entry build)

(define (first pair)
  (car pair))

(define (second pair)
  (cadr pair))

; Lookup the entry with 'name', i.e., the corresponding element in the
; entry's second list. If 'name' does not exist in the first list, invoke
; 'entry-f' with the missing name.
(define (lookup-in-entry name entry entry-f)
  (cond
    ((or (null? entry) (null? (first entry))) (entry-f name))
    ((eq? (car (first entry)) name) (car (second entry)))
    (else (lookup-in-entry name (build (cdr (first entry)) (cdr (second entry))) entry-f))))

; alternative version using helper function with descriptive names
; (as in the book)
(define (lookup-in-entry2 name entry entry-f)
  (lookup-in-entry-help name (first entry) (second entry) entry-f))

(define (lookup-in-entry-help name names values entry-f)
  (cond
    ((null? names) (entry-f name))
    ((eq? name (car names)) (car values))
    (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f))))

; A table (or environment) is a (possibly empty) list of entries.
(define (extend-table) cons)

; Find the first entry which contains 'name' in 'table'. If it does not exist,
; call table-f
(define (lookup-in-table name table table-f)
  (cond
    ((null? table) (table-f name))
    (else (lookup-in-entry
           name
           (car table)
           (lambda (name) (lookup-in-table name (cdr table) table-f))))))