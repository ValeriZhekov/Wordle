#lang scheme
(define (rand n) (floor (* n (random))))

(define words
    (list "aback" "abase" "abate" "abbey" "abyss" "acute" "adobe" "adore" "admit" "agape" "agate" "agree" "ahead" "album" "alien" "alike" "allow" "aloft" "alone" "aloud" "alpha" "altar" "amber" "ample" "angry" "aphid" "apply" "apron" "aptly" "argue" "aroma" "aside" "askew" "asset" "atoll"  "atone" "audit" "avert" "awake" "awful" "axiom" "badge" "badly" "baker" "banal" "basic" "baton" "batty" "bayou" "beady" "begin" "being" "belch" "belly" "bench" "berth" "biome" "black" "bland" "bleed" "bloke" "blown" "bluff" "blurt" "blush" "booby" "boost" "booze" "boozy" "bough" "braid" "brake" "brave" "break" "briar" "bribe" "brine" "bring" "brink" "brisk" "buggy" "cacao" "canny" "cargo" "carry" "cater" "catch" "caulk" "chafe" "champ" "chant" "charm" "cheat" "cheek" "chief" "chest" "chill" "choke" "chord" "chunk" "chute" "cigar" "cinch" "civic" "class" "clean"
          "click" "cling" "clock" "cloth" "clown" "cluck" "coast" "colon" "comet" "comma" "conic" "corny" "could" "coyly" "cramp" "crank" "crass" "crate" "craze" "crazy" "creak" "crept" "crimp" "croak" "crust" "cynic" "dandy" "death" "delta" "delve" "denim" "depot" "depth" "digit" "dodge" "donor" "doubt" "dowry" "dozen" "drain" "dream" "drink" "drive" "droll" "duchy" "dutch" "dwarf" "egret" "eject" "elder" "elope" "enema" "enjoy" "epoch" "epoxy" "equal" "erode" "error" "essay" "evade" "exist" "exult" "farce" "fault" "favor" "feast" "feign" "ferry" "fewer" "field" "finer" "first" "fixer" "fjord" "flair" "flesh" "flick" "fling" "float" "flock" "flood" "floor" "floss" "flout" "fluff" "flume" "focal" "focus" "foggy" "foray" "forge" "forgo" "forth" "found" "foyer" "frame" "fresh" "front" "froth" "fungi" "gamer" "gamma" "gaudy" "gauze" "gawky" "gecko" "girth"
          "glass" "glean" "gloat" "gloom" "glory" "glyph" "golem" "goner" "goose" "gorge" "gouge" "grade" "grate" "great" "greet" "grime" "gripe" "groin" "group" "grove" "growl" "gruel" "guild" "gully" "hairy" "happy" "hatch" "heath" "heist" "helix" "heron" "hinge" "hoard" "homer" "howdy" "humor" "humph" "hunky" "hutch" "hyper" "inane" "inept" "inert" "infer" "input" "inter" "ionic" "irony" "islet" "itchy" "ivory" "jaunt" "joust" "karma" "kebab" "khaki" "knock" "knoll" "label" "labor" "lapel" "lapse" "larva" "leave" "leery" "libel" "light" "lilac" "linen" "liver" "lofty" "loopy" "loser" "lowly" "lusty" "lying" "madam" "major" "manor" "maple" "marry" "marsh" "masse" "maxim" "medal" "merit" "metal" "midge" "midst" "mimic" "mince" "model" "moist" "money" "month" "motor" "motto" "moult" "mount" "mourn" "movie" "mummy" "naive" "nasty" "natal" "naval" "needy"
          "night" "nymph" "offal" "olive" "onset" "other" "ought" "outdo" "oxide" "panel" "panic" "paper" "parer" "parry" "patty" "pause" "peach" "perch" "perky" "phase" "photo" "picky" "piety" "pilot" "piney" "pinto" "pithy" "plant" "pleat" "pluck" "point" "poker" "pound" "power" "prick" "pride" "prime" "primo" "print" "prize" "probe" "prove" "proxy" "pulpy" "purge" "quart" "query" "quiet" "quirk" "radio" "rainy" "react" "rebus" "rebut" "recap" "renew" "repay" "retch" "retro" "rhino" "rhyme" "rival" "robin" "robot" "rogue" "roomy" "rouge" "round" "royal" "ruder" "rupee" "rusty" "saint" "salad" "saute" "scald" "scare" "scorn" "scour" "scrap" "seedy" "serve" "sever" "shake" "shall" "shame" "shard" "shawl" "shine" "shire" "shown" "showy" "shrub" "shrug" "siege" "sissy" "skill" "slosh" "sloth" "slump" "slung" "smart" "smear" "smelt" "smite" "snarl" "sneak"
          "snout" "soggy" "solar" "solve" "sonic" "sower" "spade" "spell" "spend" "spicy" "spiel" "spike" "spill" "spoke" "spray" "squad" "staff" "stair" "stale" "stand" "start" "stead" "steed" "stein" "stick" "sting" "stink" "stomp" "stool" "store" "story" "stout" "stove" "study" "sugar" "surer" "sweet" "swill" "swirl" "tacit" "tangy" "taper" "tapir" "taunt" "tease" "tepid" "their" "theme" "there" "thorn" "those" "thumb" "thyme" "tiara" "tibia" "tiger" "tilde" "tipsy" "today" "torso" "totem" "trace" "train" "trait" "trash" "trawl" "treat" "triad" "trice" "trite" "troll" "trope" "trove" "truss" "tryst" "twang" "tweed" "twice" "twine" "ulcer" "ultra" "undue" "unfed" "unfit" "unify" "unite" "unmet" "upset" "usher" "using" "usual" "usurp" "valet" "valid" "vigor" "viral" "vital" "vivid" "vodka" "voice" "vouch" "wacky" "waltz" "waste" "watch" "weary" "wedge"
          "whack" "whelp" "whoop" "wince" "woken" "wooer" "world" "woven" "wrote" "wrung" "yearn" "yield" "youth" "zesty"
          "able" "acid" "aids" "also" "anti" "area" "arms" "army" "arts" "asia" "atom" "auto" "away" "baby" "back" "bags"  "bids" "bill" "bird" "blog" "blow" "blue" "boat" "call" "came" "camp" "cape" "clip" "club" "cnet" "cock" "code" "cold" "dark" "data" "date" "dave" "diet" "disc" "disk" "does" "dogs" 
          "each" "east" "easy" "ebay" "edge" "edit" "else" "ends" "eric" "even" "ever" "eyes" "face" "fact" "feed" "feel" "fees" "fuel" "full" "fund" "hair" "half" "hall" "hand" "here" "hide" "high" "hill" "icon" "idea" "inch" "info" "into" "iowa" "ipod" "iraq" "iron" "isbn" "item" "keep" "kept" "kids" "kind" "king" "knew" "know" "kong" "ohio" "once" "ones" "only" "open" "oral" "over" "unit" "upon" "used" "user" "uses" "utah" "very" "vice" "view" "void" "vote" "year" "york" "your" "zero" "zone"))

(define numWords (length words))

(define (word? words pos)
  (if (= pos 0)
      (car words)
      (word? (cdr words) (- pos 1))))
       
(define (randWord) (word? words (rand numWords)))


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


(define (helper word len wordList green yellow grey)
  (define (choose wordList)
    (word? wordList (rand (length wordList))))
(define (help w res pos grn yel gry)
    (cond ((= 0 (string-length w)) (list grn yel gry))
          ((equal? (car res) "green") (help (substring w 1) (cdr res) (+ pos 1) (add (cons (substring w 0 1) pos) grn) yel gry))
          ((equal? (car res) "yellow") (help (substring w 1) (cdr res) (+ pos 1) grn (add (substring w 0 1) yel) gry))
          (else (help (substring w 1) (cdr res) (+ pos 1) grn yel (add (substring w 0 1) gry)))
      ))
  (let ((answer (choose wordList)))
  (begin (display answer) (newline)
         (let* ((result (read)) (colors (help answer result 0 '() '() '()))
                (newgrn (add* (car colors) green))(newyel (add* (car (cdr colors)) yellow))   (newgry (add* (car (cddr colors)) grey)))
                                (begin (display colors) (newline) (helper word len wordList newgrn newyel newgry))))))
           

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
                            ((equal? mode "helper") (helper word len (filter (lambda (x) (= (string-length x) len)) words) '() '() '()))
                            (else (begin (display "No such game mode") (newline)))))))))

(begin (display "GAME MODE:") (newline) (RUN)) 