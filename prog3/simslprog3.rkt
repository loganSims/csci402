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
    (for ((room-data rooms))
      (cond ((equal? location (car room-data))
             (displayln (cdr room-data))
             )))))

(define look
  (lambda (direction room)
    (define found #f)
    (for ((room-data rooms))
      (cond ((equal? room (car room-data))
             (for ((rooms-next-to (cdr room-data)))
               (cond ((equal? direction (car rooms-next-to))
                      (displayln (second rooms-next-to))
                      (set! found #t)))))))
    (cond ((not found)
           (displayln 'NIL)))))

(define where
  (lambda ()
    (cond ((regexp-match "-stairs" (symbol->string (car loc)))
           (display "Robbie is on the ")
           (displayln (symbol->string (car loc))))
          ((regexp-match "stairs" (symbol->string (car loc)))
           (display "Robbie is in the ")
           (displayln (symbol->string (car loc))))
          (else  
           (cond((down-stairs? loc)
                 (display "Robbie is downstairs in the ")
                 (displayln (symbol->string (car loc)))
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
    (for ((next-location (cdr location)))
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


;TEST
(set! loc (list-ref rooms 8))

;(where)
;(choices 'living-room)
;(look 'north 'pantry)
;(look 'west 'pantry)
;(look 'south 'pantry)