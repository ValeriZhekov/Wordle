#lang scheme
(define (rand n) (floor (* n (random))))

(define (testRand f n t a b c d e)
  (let ((res (f n)))
    (cond ((= t 0) (list a b c d e))
      ((= res 0) (testRand f n (- t 1) (+ a 1) b c d e))
          ((= res 4) (testRand f n (- t 1) a (+ b 1) c d e))
          ((= res 11) (testRand f n (- t 1) a b (+ c 1) d e))
          ((= res 25) (testRand f n (- t 1) a b c (+ d 1) e))
          ((= res 24) (testRand f n (- t 1) a b c d (+ e 1)))
          (else (testRand f n (- t 1) a b c d e)))))

(define words
    (list "rabbit" "wolf" "bear" "dog" "bird" "inform" "police" "vegetable" "meat" "grass" "metal"
        "hungry" "keyboard" "mouse" "backpack" "helicopter" "detective" "song" "person" "laugh" "water"
        "olive" "potato" "carrot" "apple" "pork"))

(define numWords (length words))

(define (word words pos)
  (if (= pos 0)
      (car words)
      (word (cdr words) (- pos 1))))
       
(define (randWord) (word words (rand numWords)))


(define (rateWord word input )
  
  (define (contains? letter str)
        (cond ((= 0 (string-length str)) #f)
          ((equal? letter (substring str 0 1)) #t)
          (else (contains? letter (substring str 1)))))
  
  (define (help w in) 
  (cond ((= 0 (string-length in)) '())
        ((equal? (substring w 0 1) (substring in 0 1)) (cons "green" (help (substring w 1) (substring in 1))))
        ((contains? (substring in 0 1) word) (cons "yellow" (help (substring w 1) (substring in 1))))
        (else (cons "grey" (help (substring w 1) (substring in 1))))))
             
  (help word input))
  