;; Every kind of run.
#| a block
   #| nested |# still in |#
(define-record-type point (make-point x y) point? (x point-x))

(define (f x)	; a tab before
  (if (and (> x 0) #t)
      (string-append "a \"quoted\" \\ string" "two
lines")
      (let* ((é 12) (y #xFF) (z 1.5e3))
        (list 'sym #\a -1 +inf.0 é y z))))

(λ (😀) "astral 😀" 😀) ; after astral
(define-syntax my-or (syntax-rules () ((_ a ...) (or a ...))))
"no end
