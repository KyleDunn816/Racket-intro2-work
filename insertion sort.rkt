;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |insertion sort|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define (empty-VINT? low high) (> low high))

; Insertion Sort

(define (insertion-sort! V)
  (local [
          (define (switch x y)
            (local [(define holder (vector-ref V x))]
              (begin
                (vector-set! V x (vector-ref V y))
                (vector-set! V y holder))))

            (define (insert! low high)
              (cond [(empty-VINT? low high) (void)]
                    [(<= (vector-ref V low)
                                   (vector-ref V (add1 low)))
                               (void)]
                    [else (begin (switch low (add1 low))
                                           (insert! (add1 low) high))]))

            (define (sort! low high)
              (cond [(empty-VINT? low high) (void)]
                    [else (begin
                          (sort! (add1 low) high)
                          (insert! low (sub1 high)))]))]
    (sort! 0 (sub1 (vector-length V)))))


(define V (vector 3 1 2))



(check-expect (begin
                (insertion-sort! V)
                V)
              (vector 1 2 3))



(define (display-times x)
  (cond [(> x 20000) (void)]
        [else (begin
                (display (format "Insertion Sort ~a" x))
                (newline)
                (time (insertion-sort! (build-vector x (lambda (i) (random 1000000)))))
                (newline)
                (display-times (+ x 500)))]))

(display-times 500) 