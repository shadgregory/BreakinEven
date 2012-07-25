;#lang racket/gui
;(require mzlib/list)
;(define f (new frame% (label "Frame")(width 800)(height 600)))

(define data-grid-canvas%
  (class editor-canvas%
    (super-new
     (enabled #f)
     (style '(control-border hide-hscroll hide-vscroll)))))

(define data-grid%
  (class horizontal-panel%
    (init-field parent)
    (init-field data)
    (init-field headings)
    (define sort-order "DESC")
    (define sort-by 0)
    
    (define/private (default-pick-order n)
      (pick-order n data))
    
    (define/private (pick-order n db)
      (set! db (sort db (lambda (x y) (string>? (list-ref x 0)(list-ref y 0)))))
      (if (string=? sort-order "ASC")
          (sort db (lambda (x y) (string<? (list-ref x n) (list-ref y n))))
          (sort db (lambda (x y) (string>? (list-ref x n) (list-ref y n))))))
    
    (define/public (refresh-pane db)
      (clear-grid (send this get-children))
      (set! data db)
      (new message% (parent (last (send this get-children)))(label "")(min-height 25)(horiz-margin 25))
      (re-pop-buttons (last (send this get-children)) (pick-order sort-by db))
      (re-populate-grid (send this get-children) (pick-order sort-by db)))
    
    (define/private (delete-children parent children)
      (cond
        ((null? children)(void))
        ((empty? children)(void))
        ((is-a? (car children) button%)
         (cond
           ((string=? (send (car children) get-label) "Edit")
            (send parent delete-child (car children))
            (delete-children parent (cdr children)))
           (else
            (delete-children parent (cdr children)))))
        (else 
         (send parent delete-child (car children))
         (delete-children parent (cdr children)))))
    
    (define/private (clear-grid vpans)
      (letrec ((clear (lambda (lst)
                        (cond
                          ((empty? lst)(void))
                          ((null? lst)'())
                          (else
                           (delete-children (car lst) (send (car lst) get-children))
                           (clear (cdr lst)))))))
        (clear vpans)))
    
    (define/private (re-pop-buttons vpan db)
      (cond
        ((empty? db) '())
        (else
         (new button% (label "Edit")
              (parent vpan)
              (callback (list-ref (car db)(-(length (car db)) 2))))
         (re-pop-buttons vpan (cdr db)))))
    
    (define/private (re-populate-grid vpans db)
      (cond
        ((= (length vpans) 1)(void))
        (else 
         (letrec
             ((update-grid
               (lambda (db)
                 (cond 
                   ((empty? db) (void))
                   (else              
                    (let* ((canv (new data-grid-canvas% (parent (car vpans)))) (txt (new text%)))
                      (send canv set-editor txt)
                      (send (send canv get-editor) insert (car (car db))))
                    (update-grid (cdr db)))))))
           (update-grid db))
         (re-populate-grid (cdr vpans) (treat-list db)))))
    
    (define treat-list 
      (lambda (lst)
        (cond
          ((null? lst)'())
          ((empty? lst)'())
          (else
           (cons (cdr (car lst)) (treat-list (cdr lst)))))))    
    
    (define/private (populate-grid headers db)
      (cond 
        ((empty? headers) 
         (define vpan (new vertical-panel% (parent this)(border 1)(spacing 0)))
         (new message% (parent vpan)(label "")(min-height 25)(horiz-margin 25))
         (letrec ((add-buttons (lambda (db)
                                 (cond
                                   ((empty? db) '())
                                   (else
                                    (new button% (label "Edit")(parent vpan)
                                         (callback (list-ref (car db)(-(length (car db)) 2))))
                                    (add-buttons (cdr db)))))))
           (add-buttons db)))
        (else
         (define vpan (new vertical-panel% (parent this)(border 1)(spacing 0)(min-width 195)(alignment '(left top))))
         (new button% (vert-margin 0)(horiz-margin 0)(stretchable-width #t)(parent vpan)(label (car headers))
              (callback (lambda (b evt)
                          (clear-grid (send this get-children))
                          (new message% (parent (last (send this get-children)))(label "")(min-height 25)(horiz-margin 25))
                          (if (= sort-by (- (length headings)(length headers)))
                              (cond
                                ((string=? sort-order "DESC")
                                 (set! sort-order "ASC"))
                                (else
                                 (set! sort-order "DESC")))
                              '())
                          (set! sort-by (- (length headings)(length headers)))
                          (re-pop-buttons (last (send this get-children)) 
                                          (default-pick-order (- (length headings)(length headers))))
                          (re-populate-grid (send this get-children)
                                            (default-pick-order (- (length headings)(length headers)))))))
         
         (letrec
             ((update-grid
               (lambda (db)
                 (cond 
                   ((empty? db) (void))
                   (else              
                    (let* ((canv (new data-grid-canvas% (parent vpan))) (txt (new text%)))
                      (send canv set-editor txt)
                      (send (send canv get-editor) insert (car (car db))))
                    (update-grid (cdr db)))))))
           (update-grid db))
         (populate-grid (cdr headers)(treat-list db)))))
    
    (define colnums (length headings))
    (super-new [parent parent])
    (populate-grid headings (pick-order 0 data))))

;(define my-data-grid (new data-grid% [stretchable-height #t][parent f][headings '("End Time" "Site" "Game" "Win/Loss")]
;                          [data `(("2010-02-07 21:03:09" "Full Tilt Poker" "Seven Card Stud" "$ 5.25" 
;				   ,(lambda (b evt) (display "HELLO")) "1" )
;                                  ("2010-02-20 17:43:37" "Poker Stars" "No Limit Hold'em" "$ 0.00" 
;				   ,(lambda (b evt) (display "HELLO")) "2" ))]))
;(send f show #t)
