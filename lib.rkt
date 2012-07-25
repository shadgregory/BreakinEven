#lang racket

(provide 
 standard-deviation 
 total-hours gross-winnings
 get-new-id
 data-by-month
 find-min-max
 get-position
 get-total-by-site
 site-vector-list
 games-vector-list
 hash-to-vectorlist)

(define standard-deviation
  (lambda nums
    (let* ((median (/(foldl + 0 (car nums))(length (car nums))))
           (squares-list-sum 
            (foldl + 0 (map (lambda (num) 
                              (* (- num median)(- num median)))(car nums))))
           (variance (/ squares-list-sum (length (car nums)))))
      (sqrt variance))))

(define total-hours
  (lambda (db)
    (/ (foldl (lambda (session total)
                (+ total (- (cdr (assoc 'end-time session))
                            (cdr (assoc 'start-time session))))) 0 db) 3600)))

(define gross-winnings
  (lambda (db)
    (foldl (lambda (session total)
             (+ total (- (cdr (assoc 'final session))
                         (cdr (assoc 'buyin session))))) 0 db)))

(define hash-to-vectorlist
  (lambda (h)
    (for/list ([(k v) (in-dict h)])
      (vector k v 3))))

(define get-new-id
  (lambda (db)
    (define new-id 0)
    (letrec ((find-id (lambda (lst)
                        (cond
                          ((null? lst) (add1 new-id))
                          ((empty? lst)(add1 new-id))
                          ((< new-id (cdr (assoc 'id (car lst))))
                           (set! new-id (cdr (assoc 'id (car lst))))
                           (find-id (cdr lst)))
                          (else
                           (find-id (cdr lst)))))))
      (find-id db))))

(define find-by-id
  (lambda (db id)
    (letrec
        ((find (lambda (lst)
                 (cond
                   ((null? lst) (void))
                   ((empty? lst) (void))
                   ((eqv? (cdr (assq 'id (car lst))) id)
                    (car lst))
                   (else (find (cdr lst)))))))
      (find db))))

(define data-by-month
  (lambda (alist)
    (define end (current-seconds))
    (define start (- end 31556926))
    (define current-month (date-month (seconds->date (current-seconds))))
    (define hash (make-hasheq '((1 . 0)(2 . 0)(3 . 0)(4 . 0)(5 . 0)(6 . 0)(7 . 0)(8 . 0)(9 . 0)(10 . 0)(11 . 0)(12 . 0))))
    (cond
      ((empty? alist) '())
      (else
       (letrec
           ((build
             (lambda (lst)
               (cond
                 ((null? lst) (hash-to-vectorlist hash))
                 ((empty? lst) (hash-to-vectorlist hash))
                 ((< (cdr (assoc 'end-time (car lst))) start)
                  (build (cdr lst)))
                 (else
                  (let* ((the-month (date-month (seconds->date (cdr (assoc 'end-time (car lst))))))
                         (the-total (dict-ref hash the-month)))
                    (dict-set! hash the-month
                               (+ (- (cdr (assoc 'final (car lst)))
                                     (cdr (assoc 'buyin (car lst)))) the-total))
                    (build (cdr lst))))))))
         (map (lambda (v) (vector-take v 2))(build alist)))))))

(define find-min-max
  (lambda (lst-of-vecs)
    (define return-value 0)
    (letrec
        ((find
          (lambda (lst)
            (cond 
              ((empty? lst) return-value)
              ((> (abs(dict-ref (car lst) 1)) return-value)
               (set! return-value (abs (dict-ref (car lst) 1)))
               (find (cdr lst)))
              (else (find (cdr lst)))))))
      (find lst-of-vecs))))

(define get-position
  (lambda (lst val)
    (let loop ((n 0))
      (cond
        ((= n (length lst))(void))
        ((string=? val (list-ref lst n))n)
        (else
         (loop (add1 n)))))))

(define get-total-by-site
  (lambda (site db)
    (letrec ((get-total 
	      (lambda (db)
		(cond
		 ((null? db) 0)
		 ((string=? (string-append "\"" site "\"") (cdr (assoc 'site (car db))))
		  (+ (- (cdr (assoc 'final (car db)))
			(cdr (assoc 'buyin (car db))))
          (get-total (cdr db))))
      (else
       (get-total (cdr db)))))))
      (get-total db))))

(define get-total-by-game
  (lambda (game db)
    (letrec ((get-total 
	      (lambda (db)
		(cond
		 ((null? db) 0)
		 ((string=? (string-append "\"" game "\"") (cdr (assoc 'game (car db))))
		  (+ (- (cdr (assoc 'final (car db)))
			(cdr (assoc 'buyin (car db))))
          (get-total (cdr db))))
      (else
       (get-total (cdr db)))))))
      (get-total db))))

(define site-vector-list
  (lambda (sites db)
    (letrec 
	((build
	  (lambda (s)
	    (cond
	     ((null? s) '())
	     (else
	      (cons (vector (car s) (get-total-by-site (car s) db))(build (cdr s))))))))
      (build sites))))

		     
(define games-vector-list
  (lambda (games db)
    (letrec
	((build
	  (lambda (g)
	    (cond
	     ((null? g)'())
	     (else
	      (cons (vector (car g) (get-total-by-game (car g) db))(build (cdr g))))))))
	 (build games))))
