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



(define (normal word len)

  (let ((input (read)))
    (if (not (= (string-length input) len))
        (begin (display "Wrong length") (newline) (normal word len))
        
  (let ((result (rateWord word input))) 
    (cond ((null? (filter (lambda (x)(not (equal? x "green"))) result)) (begin (display "YOU WON")))
    (else (begin (display result) (newline) (normal word len))))))))
;;cons if lst doesn't contain the value
(define (add str lst)
    (if (not (member str lst))
        (cons str lst)
        lst))
  (define (add* lst1 lst2)
    (if (null? lst1)
        lst2
        (add* (cdr lst1) (add (car lst1) lst2))))
(define (remove x lst)
    (if (equal? x (car lst))
        (cdr lst)
        (cons (car lst) (remove x (cdr lst)))))

(define (easy word len green yellow grey)
  (define (help w res pos grn yel gry)
    (cond ((= 0 (string-length w)) (list grn yel gry))
          ((equal? (car res) "green") (help (substring w 1) (cdr res) (+ pos 1) (add (cons (substring w 0 1) pos) grn) yel gry))
          ((equal? (car res) "yellow") (help (substring w 1) (cdr res) (+ pos 1) grn (add (substring w 0 1) yel) gry))
          (else (help (substring w 1) (cdr res) (+ pos 1) grn yel (add (substring w 0 1) gry)))
      ))
  
    (define (yel? w lst)
      (cond ((null? lst) #t)
            ((= (string-length w) 0) #f)
            ((member (substring w 0 1) lst) (yel? (substring w 1) (remove (substring w 0 1) lst)))
            (else (yel? (substring w 1) lst)))
          )
  (define (gry? w lst)
       (cond ((= (string-length w) 0) #t)
              ((member (substring w 0 1) lst) #f)
              (else (gry? (substring w 1) lst))))
  (define (grn? w lst)
     (cond ((null? lst) #t)
           ((not (equal? (substring w (cdr (car lst)) (+ 1 (cdr (car lst)))) (car (car lst)))) #f)
           (else (grn? w (cdr lst)))))
  
  (let ((input (read)))
    (if (not (= (string-length input) len))
        (begin (display "Wrong length") (newline) (easy word len green yellow grey))
        
  (let* ((result (rateWord word input))(colors (help input result 0 '() '() '()))
     (newgrn (add* (car colors) green))(newyel (add* (car (cdr colors)) yellow))   (newgry (add* (car (cddr colors)) grey)))
    
    (cond ((null? (filter (lambda (x)(not (equal? x "green"))) result)) (begin (display "YOU WON")))
          ((not (gry? input grey)) (begin (display "Using greys") (newline) (display (list newgrn newyel newgry)) (newline) (easy word len newgrn newyel newgry)))
          ((not (grn? input green)) (begin (display "Missing greens") (newline) (display (list newgrn newyel newgry)) (newline) (easy word len newgrn newyel newgry)))
          ((not (yel? input yellow)) (begin (display "Missing yellows") (newline) (display (list newgrn newyel newgry)) (newline) (easy word len newgrn newyel newgry)))
          
          ((not (member input words)) (begin (display "Not in wordlist") (newline) (display (list newgrn newyel newgry)) (newline)  (easy word len newgrn newyel newgry)))
          
    (else (begin (display result) (newline) (easy word len newgrn newyel newgry))))))))

(define (expert word len green yellow grey cheated?)
  (define (help w res pos grn yel gry)
    (cond ((= 0 (string-length w)) (list grn yel gry))
          ((equal? (car res) "green") (help (substring w 1) (cdr res) (+ pos 1) (add (cons (substring w 0 1) pos) grn) yel gry))
          ((equal? (car res) "yellow") (help (substring w 1) (cdr res) (+ pos 1) grn (add (substring w 0 1) yel) gry))
          (else (help (substring w 1) (cdr res) (+ pos 1) grn yel (add (substring w 0 1) gry)))
      ))
  
  
    (define (yel? w lst)
      (cond ((null? lst) #t)
            ((= (string-length w) 0) #f)
            ((member (substring w 0 1) lst) (yel? (substring w 1) (remove (substring w 0 1) lst)))
            (else (yel? (substring w 1) lst)))
          )
  (define (gry? w lst)
       (cond ((= (string-length w) 0) #t)
              ((member (substring w 0 1) lst) #f)
              (else (gry? (substring w 1) lst))))
  (define (grn? w lst)
     (cond ((null? lst) #t)
           ((not (equal? (substring w (cdr (car lst)) (+ 1 (cdr (car lst)))) (car (car lst)))) #f)
           (else (grn? w (cdr lst)))))
  
  (define (cheat w res pos)
    (cond ((null? res) '())
          ((and (not (member (substring w pos (+ pos 1)) yellow)) (not (member (substring w pos (+ pos 1)) (map car green))) (not (member pos (map cdr green))))
           (cons "grey" (cheat w (cdr res) (+ pos 1))))
          (else (cons (car res) (cheat w (cdr res) (+ pos 1))))
        ))
      
  (let ((input (read)))
    (if (not (= (string-length input) len))
        (begin (display "Wrong length") (newline) (expert word len green yellow grey cheated?))
        
  (let* ((result (rateWord word input))(colors (help input result 0 '() '() '()))
     (newgrn (add* (car colors) green))(newyel (add* (car (cdr colors)) yellow))   (newgry (add* (car (cddr colors)) grey)))
    
    (cond ((null? (filter (lambda (x)(not (equal? x "green"))) result)) (begin (display "YOU WON")))
    ((and (equal? cheated? #f) (= (rand 3) 1))  (begin (display (cheat input result 0)) (newline) (expert word len newgrn newyel newgry #t)))
    (else (begin (display result) (newline) (expert word len newgrn newyel newgry cheated?))))))))

(define modes '("normal" "easy" "helper" "expert"))
;;Задава начало
(define (RUN)
 (let ((mode (read)))
  (if (not (member mode modes))
      (begin (display "No such game mode") (newline) (RUN))
(let* ((word (randWord))(len (string-length word)))
               (begin (display word) (newline) (display "Lenght:") (display len) (newline)
                      (cond ((equal? mode "normal") (normal word len))
                            ((equal? mode "easy") (easy word len '() '() '()))
                            ((equal? mode "expert") (expert word len '() '() '() #f))
                            (else (begin (display "No such game mode") (newline)))))))))

(begin (display "GAME MODE:") (newline) (RUN)) 