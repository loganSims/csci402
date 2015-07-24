#lang racket
(require racket/include)
(include "Program-3-data.rkt")

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
           (displayln (symbol->string (car loc))))
          ((regexp-match "stairs" (symbol->string loc))
           (display "Robbie is in the ")
           (displayln (symbol->string (car loc))))
          (else  
           (cond((down-stairs? loc)
                 (display "Robbie is downstairs in the ")
                 (displayln (symbol->string loc))
                 )
                (else
                (display "Robbie is upstairs in the ")
                 (displayln (symbol->string (car loc)))
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
    (define dest '(Ouch! Robbie hit the wall))
    (cond ((not (equal? (look direction loc) 'NIL))
           (for ((room rooms))
             (cond ((equal? (car room) (look direction loc))
                    (set-robbie-location (car room))
                    (set! dest (where)))))))
    dest))

(define traverse
  (lambda (src dest)
    (displayln "traversing..")
    
    ))



(set-robbie-location 'kitchen)
(traverse loc 'library)











;TEST
;(set-robbie-location 'kitchen)

;(displayln ">(where)")
;(where)
;(displayln ">(choices loc)")
;(choices loc)
;(displayln ">(move 'north)")
;(move 'north)
;(displayln ">(move 'west)")
;(move 'west)
;(displayln ">(choices loc)")
;(choices loc)
;(displayln ">(move 'south)")
;(move 'south)
;(displayln "(where)")
;(where)