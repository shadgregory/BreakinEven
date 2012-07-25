#lang racket/base
(require rackunit "lib.rkt")

(check-equal? (standard-deviation '(13 22 97 44 8 156 23)) 50.85393248294169)
(check-equal? (gross-winnings '(((buyin . 3)(final . 2))
                                ((buyin . 4)(final . 3)))) -2)
(check-equal? (total-hours '(((start-time . 0)(end-time . 3600))
                             ((start-time . 3600)(end-time . 7200)))) 2)
(check-equal? (get-total-by-site 
	       "\"Poker Stars\"" 
	       '(((buyin . 3.5) (final . 2) (site . "\"Poker Stars\""))
		 ((buyin . 3.5) (final . 4.0) (site . "\"Poker Stars\""))
		 ((buyin . 5.5) (final . 0) (site . "\"Full Tilt Poker\""))
		 ((buyin . 5.5) (final . 9) (site . "\"Poker Stars\"")))) 2.5)
