#lang racket

;; this is the code for problem set -- Lunar Lander

; terminal i/o primitives supported in MIT Scheme but not DrScheme -----------

(define write-line
  (lambda (line)
    (begin
      (display line)
      (newline))))

(define prompt-for-command-char 
  (lambda (prompt)
    (begin
        (display prompt)
        (read))))

; Lunar Landar functions -----------------------------------------------------


; Problem 1 ------------------------------------------------------------------


(define make-ship-state ; Constructor
  (lambda(height velocity fuel)
    (list height velocity fuel))) 

(define height ; Selector
  (lambda(ship-state)
    (list-ref ship-state 0)))

(define velocity ; Selector
  (lambda(ship-state)
    (list-ref ship-state 1)))

(define fuel ; Selector
  (lambda(ship-state)
    (list-ref ship-state 2)))

; Problem 2,8----------------------------------------------------

(define update 
  (lambda (ship-state fuel-burn-rate)
    (let ((new-burn-rate ; Don't let burn-rate above 1
           (if (< ( fuel ship-state) 1 )
               (fuel ship-state)
               (min fuel-burn-rate 1))))
      (display (list fuel-burn-rate new-burn-rate)) ; Debugging 
      (make-ship-state
     (+ (height ship-state) (* (velocity ship-state) dt)) ; height
     (+ (velocity ship-state)
        (* (- (*  engine-strength new-burn-rate) gravity)
           dt))                                           ; velocity
     (- (fuel ship-state) (* new-burn-rate dt))))))       ; fuel



; Problem 3-----------------------------------------------------

(define full-burn (lambda (ship-state) 1)) ;use fuel every iteration
(define no-burn (lambda (ship-state) 0)) ; use no fuel every iteration

(define ask-user (lambda (ship-state) (get-burn-rate))) 

  
(define lander-loop
  (lambda (ship-state strategy)
    (show-ship-state ship-state)
    (if (landed? ship-state)
        (end-game ship-state)
        (lander-loop (update ship-state (strategy ship-state)) strategy))))

(define play 
  (lambda (strategy)
    (lander-loop (initial-ship-state) strategy)))


; Problem 4----------------------------------------------------

;(define random-choice
;  (lambda (strategy-1 strategy-2)
;  (lambda (ship-state)
;    (if (= (random 2) 0) ;  1 will mean True, 0 False
;        (strategy-1 ship-state)
;        (strategy-2 ship-state)))))

(define random-choice
  (lambda (strategy-1 strategy-2)
    (choice strategy-1
            strategy-2
            (lambda (ship-state) (= (random 2) 0)))))


; Problem 5---------------------------------------------------

;(define height-choice
;  (lambda(strategy-1 strategy-2 height-limit)
;    (lambda(ship-state)
;      (if(<=(height ship-state) height-limit)
;         (strategy-2 ship-state)
;         (strategy-1 ship-state)))))

(define height-choice
  (lambda (strategy-1 strategy-2 height-limit)
    (choice strategy-1
            strategy-2
            (lambda (ship-state) (cond ((>= (height ship-state) height-limit) #t)
                                       (else #f))))))


; Problem 6---------------------------------------------------

(define choice
  (lambda (strategy-1 strategy-2 condition)
    (lambda (ship-state)
      (if (eq? (condition ship-state) #t)
          (strategy-1 ship-state)
          (strategy-2 ship-state)))))


; Problem 7---------------------------------------------------

(define square (lambda (x) (* x x ))) ; raises a given value to the power of 2

(define get-acc (lambda (ship-state)
                  (/(square (velocity ship-state)) (* 2 (height ship-state))))) ; gets the acceleration with given equation

(define constant-acc ;uses enough engine strength to counter-act gravity acceleration
  (lambda (ship-state)
    (/(+ (get-acc ship-state) gravity) engine-strength))) 

; Problem 9---------------------------------------------------

(define optimal-constant-acc ; always lands rocket successfully and waits until proper height to start using fuel 
  (lambda (ship-state)
    ( cond((<(- 1 (constant-acc ship-state)) 0.2)
           (constant-acc ship-state))
          (else (no-burn ship-state)))))




(define show-ship-state 
  (lambda (ship-state)
    (write-line 
      (list 'height (height ship-state)
            'velocity (velocity ship-state)
            'fuel (fuel ship-state)))))

(define landed? 
  (lambda (ship-state)
    (<= (height ship-state) 0)))

(define end-game 
  (lambda (ship-state)
    (let ((final-velocity (velocity ship-state)))
         (write-line final-velocity)
         (cond ((>= final-velocity safe-velocity)
                 (write-line "good landing")
                 'game-over)
               (else
                 (write-line "you crashed!")
                 'game-over)))))

(define get-burn-rate
  (lambda ()
    (if (eq? (player-input) burn-key)
        1
        0)))



; making initial-ship-state a function allows make-ship-state to remain
; unwritten without causing a load-time error
(define initial-ship-state 
  (lambda ()
    (make-ship-state 50       ; 50 km high
                     0        ; not moving (0 km/sec)
                     20)))    ; 20 kg of fuel left

(define dt 1)               ; 1 second interval of simulation
  
(define gravity 0.5)        ; 0.5 km/sec/sec
  
(define safe-velocity -0.5) ; 0.5 km/sec or faster is a crash

(define engine-strength 1)  ; 1 kilonewton-second

(define player-input
  (lambda ()
    (prompt-for-command-char " action: ")))

(define burn-key 'b)

