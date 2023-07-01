;(define samples
;  (mh-query 1000 100

;; set the memoized core strength of each team, regardless of match played
(define strength-core (mem (lambda (team)
  (abs (gaussian 2.0 1.0)))))

;; set the memoized strength of each team for a specific match index, modifying the core strength
(define strength-match (mem (lambda (team match)
  (if (eq? team 'none)
    0
    (+ (strength-core team) (gaussian 0.5 0.2))))))

;; to evaluate the strength of a team vector, get the match strength of the team represented by the vector
(define (team-strength team-vector match)
  (sum
    (map (lambda (x) (strength-match x match)) team-vector)
  )
)

;; calculate which team wins, winning/drawing teams get points in their position in the vector
(define (play-match team-vector-1 team-vector-2 match-1 match-2)
  ; goals for each team modeled by Poisson distribution, depending on match index
  (let ((team1-goals (poisson (team-strength team-vector-1 match-1))) (team2-goals (poisson (team-strength team-vector-2 match-2))))
    (if (= team1-goals team2-goals)
      ; draw: 1 point for each team (create each team score vector, then add vectors)
      ; we just use strength-match to get a vector of numbers where 0 denotes team not playing
      ; so we can call that on match index 1 because the specific strength doesn't matter
      (map +
        (map (lambda (x) (if (> x 0) 1 0)) (map (lambda (y) (strength-match y 1)) team-vector-1))
        (map (lambda (x) (if (> x 0) 1 0)) (map (lambda (y) (strength-match y 1)) team-vector-2))
      )
      (if (> team1-goals team2-goals)
        ; win: 3 points for winning team
        (map (lambda (x) (if (> x 0) 3 0)) (map (lambda (y) (strength-match y 1)) team-vector-1))
        (map (lambda (x) (if (> x 0) 3 0)) (map (lambda (y) (strength-match y 1)) team-vector-2))
      )
    )
  )
)

;; helper function to combine leading element with each element in the list by creating sub-lists
(define (combine-element-with-list element lst)
  (map (lambda (x) (list element x)) lst))

;; creates tournament triangle, recursively: all teams playing against each other once
(define (create-tournament teams)
  (if (= (length teams) 2)
    ; down to the last two teams: create a match among them and stop the recursion
    (list (list (first teams) (last teams)))
    ; have the first team in the list play against all remaining teams
    ; then play all matches among teams that don't include the first team
    (append (combine-element-with-list (first teams) (rest teams)) (create-tournament (rest teams)))
  )
)

;; create a team vector for each team in teams, for a total of number teams - vector gives each team its own index
(define (create-team-vectors teams number)
  (if (= (length teams) 1)
    (list (update-list (make-list number 'none) (- number 1) (first teams)))
    (append (list (update-list (make-list number 'none) (- number (length teams)) (first teams))) (create-team-vectors (rest teams) number))
  )
)

;(condition (= 3 (first (play-tournament (create-tournament (create-team-vectors (list 'ger 'fra 'arg 'jpn) 4))))))
;(strength 'ger)

;; creates number triangle, recursively: all numbers from 1 to m, once
(define (triangle n m)
  (if (= n (- m 1))
    (list (list n (- m 1)))
    (append (map (lambda(x) (list n (- x 1))) (range (+ n 1) m)) (triangle (+ n 1) m))
  )
)

;; combines the triangle of matches with the triangle of match indexes
(define (combine-with-index matches group-size)
  (define (combine index match)
    (list match index))
  (map combine (triangle 1 group-size) matches))

(map (lambda (x) (play-match (first (first x)) (second (first x)) (first (second x)) (second(second x))))
  (combine-with-index (create-tournament (create-team-vectors (list 'ger 'fra 'arg 'jpn) 4)) 4))

;))
;(density samples "Germany Strength" true)
