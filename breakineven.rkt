#!/usr/bin/env gracket
#lang scheme/gui
(require scheme/date)
(require framework)
(require plot)
(require mzlib/list)
(include "data-grid.rkt")
(require "lib.rkt")
(require srfi/48)

(define tmp-db '())
(define *db* '())
(date-display-format 'iso-8601)
(define g (open-input-file "games.rkt"))
(define *games* (read g))
(define *sites* (read (open-input-file "sites.rkt")))

;;override key events for frame
(define ctrlx #f)
(define be-frame%
  (class* frame% ()
    (define/override (on-subwindow-char window event)
      (let*
          ((c (send event get-key-code)))
        (cond
          ((and (eq? c #\x)(boolean=? (send event get-control-down) #t))
           (set! ctrlx #t))
          ((and (eq? c #\c)
                (boolean=? (send event get-control-down) #t)
                (boolean=? ctrlx #t))
           (exit #t))
          ((and (eq? c #\1)
                (boolean=? (send event get-control-down) #t))
           (set! ctrlx #f)
           (send tab-panel set-selection 0)
           (show-summary))
          ((and (eq? c #\2)
                (boolean=? (send event get-control-down) #t))
           (set! ctrlx #f)
           (send tab-panel set-selection 1)
           (show-sessions))
          ((and (eq? c #\3)
                (boolean=? (send event get-control-down) #t))
           (set! ctrlx #f)
           (send tab-panel set-selection 2)
           (show-history))
          ((and (eq? c #\4)
                (boolean=? (send event get-control-down) #t))
           (set! ctrlx #f)
           (send tab-panel set-selection 3)
           (show-graphs))
          ((eq? c 'release)
           (super on-subwindow-char window event))
          (else
           (set! ctrlx #f)
           (super on-subwindow-char window event)))))
    (super-instantiate ())))

(define frame (new be-frame% [label "Breaking Even"][width 800][height 600]))
(define-struct session (id start-time end-time buyin final site game))
;;need to put this stuff in an init func
(cond
  ((file-exists? "sessions.rkt")
   (call-with-input-file "sessions.rkt"
     (lambda (in)
       (set! tmp-db (read in)))))
  (else '()))

(cond
  ((eof-object? tmp-db)
   (void))
  ((empty? tmp-db)
   (void))
  (else
   (map (lambda (session)
          (set! *db* (append *db* `((,(assoc 'start-time session)                                  
                                     ,(assoc 'end-time session)
                                     ,(assoc 'buyin session)
                                     ,(assoc 'final session)
                                     (site . ,(string-append "\"" 
                                                             (cdr (assoc 'site session))
                                                             "\""))
                                     (game . ,(string-append "\""
                                                             (cdr (assoc 'game session)) 
                                                             "\""))
                                     ,(assoc 'id session)))))) tmp-db)))
(define show-summary
  (lambda ()
    (send graphs-pane show #f)
    (send sessions-pane show #f)
    (send graphs-pane show #f)
    (send data-grid-pane show #f)
    (send data-grid-pane min-height 110)
    (send sessions-pane min-height 110)
    (send graphs-pane min-height 110)
    (send summary-pane show #t)))

(define show-sessions
  (lambda ()
    (send current-label-pane show #t)
    (send sessions-pane show #t)
    (send graphs-pane show #f)
    (send summary-pane show #f)
    (send sessions-pane min-height 110)
    (send graphs-pane min-height 110)
    (send data-grid-pane min-height 110)
    (send data-grid-pane show #f)))

(define show-history
  (lambda ()
    (send current-label-pane show #f)
    (send sessions-pane show #f)
    (send graphs-pane show #f)
    (send summary-pane show #f)
    (send data-grid-pane min-height 540)
    (send data-grid-pane show #t)))

(define show-graphs
  (lambda ()
    (send current-label-pane show #f)
    (send sessions-pane show #f)
    (send summary-pane show #f)
    (send graphs-pane show #t)
    (send sessions-pane min-height 540)
    (send data-grid-pane show #f)))


(define tab-panel (new tab-panel% [choices '("Summary" "Sessions" "History" "Graphs")][parent frame]
                       (callback(lambda (tp e)
                                  (cond
                                    ((eq? (send tp get-selection) 0)
                                     (show-summary))
                                    ((eq? (send tp get-selection) 1)
                                     (show-sessions))
                                    ((eq? (send tp get-selection) 2)
                                     (show-history))
                                    (else
                                     (show-graphs)))))))
(define outer-swapping-panel (instantiate horizontal-panel% ()
                               (parent tab-panel)
                               (stretchable-height #f)))

(define spacing-panel (instantiate horizontal-panel% ()
                        (stretchable-width #f)
                        (parent outer-swapping-panel)
                        (min-width 20)))

(define swapping-panel (instantiate panel:single% ()
                         (parent outer-swapping-panel)
                         (alignment '(left center))
                         (stretchable-width #t)
                         (stretchable-height #f)))

(define data-grid-pane (new panel% [parent swapping-panel][stretchable-width #t] 
                            [stretchable-height #t][alignment '(left top)]))
(send data-grid-pane show #f)
(define summary-pane (new horizontal-panel% (parent swapping-panel)(alignment '(center top))))
(define graphs-pane (new horizontal-panel% [parent swapping-panel][alignment '(left top)]))
(define sessions-pane (new vertical-panel% [parent swapping-panel][min-height 110][alignment '(left top)]))
(define current-label-pane (new horizontal-panel% [parent sessions-pane][alignment '(left top)]))
(define vertical-label-pane (new vertical-panel% [parent current-label-pane][alignment '(left top)]))
(define start-button (new button% 
                          [parent vertical-label-pane]
                          [label "Start Session"]
                          (callback (lambda (b e) (session-event b e)))))
(define current-label (new message% [parent vertical-label-pane]
                           [label "Active Games: "]
                           [font (send the-font-list
                                       find-or-create-font
                                       10
                                       (send normal-control-font get-family)
                                       (send normal-control-font get-style)
                                       'bold
                                       (send normal-control-font get-underlined)
                                       (send normal-control-font get-smoothing))]))

(define error-dialog
  (lambda (msg)
    (define d (new dialog% [parent frame][label "Error"][width 200][height 125]))
    (new message% [parent d][label msg])
    (new button% [parent d][label "OK"][callback (lambda (b e)(send d show #f))])
    (send d show #t)))

(define question-dialog
  (lambda (msg cb)
    (define d (new dialog% [parent frame][label "Error"][width 200][height 100]))
    (new message% [parent d][label msg])
    (define hpan (new horizontal-panel% (parent d)(alignment '(center bottom))))
    (new button% [parent hpan][label "OK"][callback (lambda (b e)(cb b e)(send d show #f))])
    (new button% [parent hpan][label "Cancel"][callback (lambda (b e)(send d show #f))])
    (send d show #t)))

(define session-event
  (lambda (m e)
    (define start-time (current-seconds))
    (define panel (new horizontal-panel% [parent sessions-pane][alignment '(left top)]))
    (new button% [parent panel][label "END"]
         (callback (lambda (b e)
                     (cond
                       ((string=? (send (third (send panel get-children)) get-value) "")               
                        (error-dialog "Buyin is required."))
                       ((string=? (send (fourth (send panel get-children)) get-value) "")
                        (error-dialog "Final is required."))
                       (else                       
                        (set! *db*
                              (append *db*
                                      `(((id . ,(get-new-id *db*))
                                         (start-time . ,start-time)
                                         (end-time . ,(current-seconds))
                                         (buyin . ,(string->number (send (third 
                                                                          (send panel get-children)) get-value)))
                                         (final . ,(string->number (send (fourth 
                                                                          (send panel get-children)) get-value)))
                                         (site . , (string-append 
                                                    "\"" 
                                                    (send (fifth 
                                                           (send panel get-children)) get-string-selection) "\""))
                                         (game . ,(string-append "\""
                                                                 (send
                                                                  (second
                                                                   (send panel get-children))
                                                                  get-string-selection)"\""))))))
                        (define p (open-output-file "sessions.rkt" #:exists 'replace))
                        (display *db* p)
                        (close-output-port p)
                        (send sessions-pane delete-child panel)
                        (send my-data-grid refresh-pane (get-session-values *db*))
                        (refresh-summary)
                        (refresh-graph))))))
    (new choice% [parent panel][label "Games"][choices *games*])
    (new text-field% [parent panel][label "Buy-in"])
    (new text-field% [parent panel][label "Final"])
    (new choice% [parent panel][label "Site"][choices *sites*])
    (new button% [parent panel][label "DELETE"] (callback (lambda (b e)
                                                            (send sessions-pane delete-child panel))))))

(define get-session-values
  (lambda (alist)
    (map (lambda (session)
           `(
             ,(date->string (seconds->date (cdr (assoc 'end-time session))) #t)             
             ,(regexp-replace* "\\\"" (cdr(assoc 'site session)) "")
             ,(regexp-replace* "\\\"" (cdr(assoc 'game session)) "")
             ,(string-append "$ " (format "~1,2F"  (- (cdr(assoc 'final session))
                                                      (cdr (assoc 'buyin session)))))
             ,(lambda (b evt) 
                (let ((d (new dialog% (parent frame)(label "Session")(width 200)(height 200))))
                  (let ((buyin (new text-field% (parent d)(label "Buy-in")
                                    (init-value (number->string (cdr (assoc 'buyin session))))))
                        (site  (new choice% [parent d][label "Site"]
                                    [choices *sites*](selection 
                                                      (get-position 
                                                       *sites* 
                                                       (regexp-replace* 
                                                        "\\\"" (cdr (assoc 'site session))"")))))
                        (game (new choice% [parent d][label "Games"]
                                   [choices *games*]
                                   (selection 
                                    (get-position *games* 
                                                  (regexp-replace* 
                                                   "\\\"" 
                                                   (cdr (assoc 'game session))"")))))
                        (final (new text-field% [parent d][label "Final"]  
                                    (init-value (number->string(cdr (assoc 'final session))))))
                        (button-panel (new horizontal-panel% 
                                           (parent d)
                                           (alignment 
                                            '(center center)))))                                      
                    (new button% [parent button-panel][label "OK"]
                         [callback (lambda (b e)
                                     (update-db (make-session 
                                                 (cdr (assoc 'id session))
                                                 (cdr (assoc 'start-time session))
                                                 (cdr (assoc 'end-time session))
                                                 (send buyin get-value)
                                                 (send final get-value)
                                                 (string-append 
                                                  "\""
                                                  (send site get-string-selection)"\"")
                                                 (string-append 
                                                  "\""
                                                  (send game get-string-selection)"\"")))
                                     (save-and-refresh *db*)
                                     (send d show #f))])
                    (new button% (parent button-panel)(label "Delete")
                         (callback (lambda (b e)
                                     (question-dialog "Are you sure you want to delete?"
                                                      (lambda (b e)
                                                        (delete-session (make-session 
                                                                         (cdr (assoc 'id session))
                                                                         (cdr (assoc 'start-time session))
                                                                         (cdr (assoc 'end-time session))
                                                                         (send buyin get-value)
                                                                         (send final get-value)
                                                                         (string-append
                                                                          "\""
                                                                          (send site get-string-selection)"\"")
                                                                         (string-append
                                                                          "\""
                                                                          (send game get-string-selection)"\"")))
                                                        (save-and-refresh *db*)
                                                        (send d show #f))))))
                    (new button% (parent button-panel)(label "Cancel")(callback (lambda (b e) 
                                                                                  (send d show #f)))))
                  (send d show #t)))
             ,(number->string (cdr (assoc 'id session))))) alist)))

(define save-and-refresh
  (lambda (db)
    (let ((p (open-output-file "sessions.rkt" #:exists 'replace)))
      (display db p)
      (close-output-port p)
      (send my-data-grid refresh-pane (get-session-values db))
      (refresh-summary)
      (refresh-graph))))

(define update-db
  (lambda (a-session)
    (set! *db* (append (filter (lambda (obj) (not (eqv? (session-id a-session)
                                                        (cdr (assoc 'id obj))))) *db*)
                       `(((start-time . ,(session-start-time a-session))
                          (end-time . ,(session-end-time a-session))
                          (buyin . ,(string->number(session-buyin a-session)))
                          (final . ,(string->number(session-final a-session)))
                          (site . ,(session-site a-session))
                          (game . ,(session-game a-session))
                          (id . ,(session-id a-session))))))))

(define delete-session
  (lambda (a-session)
    (set! *db* (filter (lambda (obj)
                         (not (eqv? (session-id a-session)
                                    (cdr (assoc 'id obj))))) *db*))))

(define my-data-grid (new data-grid% [parent data-grid-pane]
                          [headings '("End Time" "Site" "Game" "Win/Loss")]
                          [min-width 755]
                          [data (get-session-values *db*)]))
(define t (new text%))
(define ec (new editor-canvas% (editor t) (parent graphs-pane)))
(new choice% [parent graphs-pane][label "Site"][choices '("By Month" "By Site" "By Game")]
     (callback(lambda (tp e)
                (cond
                  ((eq? (send tp get-selection) 0)
                   (refresh-graph))
                  ((eq? (send tp get-selection) 2)
                   (game-graph))
                  ((eq? (send tp get-selection) 1)
                   (site-graph))))))
(define summary-canvas (new editor-canvas%
                            (editor (new text%))(enabled #f) (parent summary-pane)
                            (style '(no-hscroll no-vscroll))))

(define refresh-summary
  (lambda ()
    (send (send summary-canvas get-editor) select-all)
    (send (send summary-canvas get-editor) clear)
    (send (send summary-canvas get-editor) insert (string-append 
                                                   "Net Win/Loss: " 
                                                   (real->decimal-string (gross-winnings *db*)))  0)
    (send (send summary-canvas get-editor) insert "\n")
    (send (send summary-canvas get-editor) insert (string-append 
                                                   "Total Hours: " 
                                                   (real->decimal-string (total-hours *db*) 2)))
    (send (send summary-canvas get-editor) insert "\n")
    (send (send summary-canvas get-editor) insert
          (string-append "Standard Deviation: "
                         (format "~1,2F" 
                                 (real->decimal-string 
                                  (standard-deviation 
                                   (map
                                    (lambda (session)
                                      (string->number
                                       (regexp-replace
                                        "^\\$[ ]*"
                                        (fourth session) "")))
                                    (get-session-values *db*)))))))))
(refresh-summary)

(define game-graph
  (lambda ()
    (send (send ec get-editor) select-all)
    (send (send ec get-editor) clear)
    (send graphs-pane delete-child (second (send graphs-pane get-children)))
    (new choice% [parent graphs-pane](selection 2)[label "Game"]
         [choices '("By Month" "By Site" "By Game")]
         (callback(lambda (tp e)
                    (cond
                      ((eq? (send tp get-selection) 0)
                       (refresh-graph))
                      ((eq? (send tp get-selection) 2)
                       (game-graph))
                      ((eq? (send tp get-selection) 1)
                       (site-graph))))))
    (print (plot 
            (list 
             (discrete-histogram 
              (games-vector-list *games* *db*)
              #:y-max (+ 1 (andmap
                            max (map abs (map (lambda (v) (vector-ref v 1))
                                              (games-vector-list *games* *db*)))))
              #:y-min (- 0 (+ 1 (andmap max 
                                        (map abs 
                                             (map 
                                              (lambda (v) 
                                                (vector-ref v 1)) 
                                              (site-vector-list *sites* *db*))))))))) out)))

(define site-graph
  (lambda ()
    (send (send ec get-editor) select-all)
    (send (send ec get-editor) clear)
    (send graphs-pane delete-child (second (send graphs-pane get-children)))
    (new choice% [parent graphs-pane](selection 1)[label "Site"]
         [choices '("By Month" "By Site" "By Game")]
         (callback(lambda (tp e)
                    (cond
                      ((eq? (send tp get-selection) 0)
                       (refresh-graph))
                      ((eq? (send tp get-selection) 2)
                       (game-graph))
                      ((eq? (send tp get-selection) 1)
                       (site-graph))))))
    (print (plot 
            (list 
             (discrete-histogram 
              (site-vector-list *sites* *db*)
              #:y-max (+ 1 (andmap
                            max (map abs (map (lambda (v) (vector-ref v 1))
                                              (site-vector-list *sites* *db*)))))
              
              #:y-min (- 0 (+ 1 (andmap max 
                                        (map abs 
                                             (map 
                                              (lambda (v) 
                                                (vector-ref v 1)) 
                                              (site-vector-list *sites* *db*))))))))) out)))

(define refresh-graph
  (lambda ()
    (send (send ec get-editor) select-all)
    (send (send ec get-editor) clear)
    (send graphs-pane delete-child (second (send graphs-pane get-children)))
    (new choice% [parent graphs-pane][label "Site"][choices '("By Month" "By Site" "By Game")]
         (callback(lambda (tp e)
                    (cond
                      ((eq? (send tp get-selection) 0)
                       (refresh-graph))
                      ((eq? (send tp get-selection) 1)
                       (site-graph))
                      ((eq? (send tp get-selection) 2)
                       (game-graph))))))
    (print (plot 
            (lines (data-by-month *db*) #:color 'green #:width 2)
            #:y-min (- (* -1 (find-min-max (data-by-month *db*))) 1) 
            #:y-max (+ (find-min-max (data-by-month *db*)) 1) 
            #:x-min 1 
            #:x-max 12 #:y-label "Winnings" #:x-label "Month") out)))

(define (make-editor-output-port text)
  (make-output-port text
                    always-evt
                    (lambda (s start end non-block? breakable?)
                      (send text insert
                            (bytes->string/latin-1 s #f start end))
                      (- end start))
                    void
                    (lambda (special buffer? breakable?)
                      (send text insert special))))

(define (wrap-port-handler old-handler)
  (lambda (value port)
    (write-special value port)
    (old-handler value port)))

(define out (make-editor-output-port t))

(port-display-handler out
                      (wrap-port-handler (port-display-handler out)))
(port-print-handler out
                    (wrap-port-handler (port-print-handler out)))
(port-write-handler out
                    (wrap-port-handler (port-write-handler out)))

(print (plot 
        (lines 
         (data-by-month *db*)
         #:color 'green
         #:width 2)
        #:y-min (- (* -1 (find-min-max (data-by-month *db*))) 1) 
        #:y-max (+ (find-min-max (data-by-month *db*)) 1) 
        #:x-min 1 
        #:x-max 12 #:y-label "Winnings" #:x-label "Month") out)

(send frame set-icon (make-object bitmap% "breakineven.png" 'png) #f 'both)
(send frame show #t)
(send summary-pane show #t)

