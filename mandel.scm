#!/usr/bin/csi -s

(define MAX_ITERATIONS 27)
(define OUTPUT_RES_X 3.5)
(define OUTPUT_RES_Y 2)
(define OUTPUT_OFFSET_X -2.5)
(define OUTPUT_OFFSET_Y -1)
(define RESOLUTION_X 150)
(define RESOLUTION_Y 60)

(define (for start end fn)
    (if (>= start end)
        #t
        (begin
            (fn start)
            (for (+ 1 start) end fn))))

; turn # interactions into a char for display
(define (show val) 
    (if (<= val 26)
        (integer->char (+ val 
                          -1
                          (char->integer #\A)))
        " "))
; Calculate how many iterations are required.
(define (calc x y) 
    (let (
      (xi (+ OUTPUT_OFFSET_X (* OUTPUT_RES_X (/ x RESOLUTION_X))))
      (yi (+ OUTPUT_OFFSET_Y (* OUTPUT_RES_Y (/ y RESOLUTION_Y)))))
    (define (calc_inner x y iters) 
        (if (or (>= iters MAX_ITERATIONS)
                (< 4 (+ (* x x) (* y y))))
            iters
            (let (
                  (xnext (+ xi (* x x) (* -1 y y)))
                  (ynext (+ yi (* x y 2))))
                (calc_inner xnext ynext (+ 1 iters)))))
    (calc_inner 0 0 0)))
(define (draw_row y width)
    (for 0 width (lambda (x)
        (display (show (calc x y))))))
(define (draw_window width height) 
    (for 0 height (lambda (y)
        (begin
            (draw_row y width)
            (display "\n")))))
        

(draw_window RESOLUTION_X RESOLUTION_Y)


