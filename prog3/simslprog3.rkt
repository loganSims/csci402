#lang racket
(require racket/include)
(include "Program-3-data.rkt")

(define node% 
  (class object%
    (init r)               
    (define room r)
    (define parent '())
    (define direction '())
    (define/public (get-room)
    room)
    (define/public (get-parent)
    parent)
    (define/public (get-direction)
    direction)
    (define/public (set-parent p)
    (set! parent p)) 
    (define/public (set-direction p)
    (set! direction p))    
    (super-new)))

;global for robbie's location
(define loc '())

;Moves Robbie to PLACE by setting
;the variable LOC.
(define set-robbie-location 
  (lambda (location)
    (set! loc location)))

(define choices
  (lambda (location)
    (define choices '())
    (for ((room-data rooms))
      (cond ((equal? location (car room-data))
             (set! choices (cdr room-data))
             )))
      choices))

(define look
  (lambda (direction room)
    (define dest 'NIL)
    (for ((room-data rooms))
      (cond ((equal? room (car room-data))
             (for ((rooms-next-to (cdr room-data)))
               (cond ((equal? direction (car rooms-next-to))
                      (second rooms-next-to)
                      (set! dest (second rooms-next-to))))))))
    dest))

(define where
  (lambda ()
    (cond ((regexp-match "-stairs" (symbol->string loc))
           (display "Robbie is on the ")
           (displayln (symbol->string loc)))
          ((regexp-match "stairs-" (symbol->string loc))
           (display "Robbie is in the ")
           (displayln (symbol->string loc)))
          (else  
           (cond((down-stairs? loc)
                 (display "Robbie is downstairs in the ")
                 (displayln (symbol->string loc))
                 )
                (else
                (display "Robbie is upstairs in the ")
                 (displayln (symbol->string loc))
                 ))))))

;determines if a location is down-stairs.
;Used by Where for printing location
(define down-stairs?
  (lambda (location)
    (define downstairs '())
    (for ((next-location (choices location)))
      (cond ((equal? downstairs '())
             (cond ((regexp-match "downstairs" (symbol->string (second next-location)))
                    (set! downstairs #t))
                   ((regexp-match "upstairs" (symbol->string (second next-location)))
                    (set! downstairs #f)))
             (cond ((and (not (regexp-match "-stairs" (symbol->string (second next-location))))
                         (equal? downstairs '()))
                    (for ((room rooms))
                      (cond ((equal? next-location (car room))
                             (set! downstairs (down-stairs? room))))))))))
    downstairs))


(define move
  (lambda (direction)
    (define moved #f)
    (define dest '(Ouch! Robbie hit the wall))
    (cond ((not (equal? (look direction loc) 'NIL))
           (for ((room rooms))
             (cond ((and (equal? (car room) (look direction loc)) (not moved))
                    (set-robbie-location (car room))
                    (set! dest (where))
                    (set! moved #t))))))
    dest))

; Uses BFS to find shortest path from src to dest
;
(define traverse
  (lambda (src dest)
    (define root (new node% (r src)))
    (define currNode root)
    (define newNode '())    
    (define v (list src))

    (define Q '())
    (define solution '())
    (set! Q (append (list root) Q))
    
    (let loop ()
      (cond((not (empty? Q))
            (set! currNode (car Q))
            (set! Q (cdr Q))
            (for ((next-location (choices (send currNode get-room))))
              (cond ((not(member (list-ref next-location 1) v))
                     (set! v (append v (cdr next-location)))
                     (set! newNode (new node% (r (list-ref next-location 1))))
                     (send newNode set-parent currNode)
                     (send newNode set-direction (list-ref next-location 0))
                     (set! Q (append (list newNode) Q))
                     (cond ((equal? (list-ref next-location 1) dest)
                            (set! solution newNode)
                            (set! Q '()))))))
            (loop))))
    (print-path solution)))

;Given a node the contains the dest, recursivly
;travels up parents to print the path from src to dest.
(define print-path
  (lambda (node)
  (cond ((not(equal? (send node get-parent) '()))
         (print-path (send node get-parent))))
  (cond ((not (equal? (send node get-direction) '()))
         (display "Go ")
         (display (send node get-direction))
         (display " to ")
         (displayln (send node get-room))))))


;TESTING;;;;;;;;;;;;;;;;;;;

(set-robbie-location 'kitchen)
(traverse loc 'library)

(move 'south)
(move 'west)
(move 'west)
(move 'north)
(move 'north)