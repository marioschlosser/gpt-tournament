;; helper function that adds two lists by adding each element (vector addition)
(define (add-lists lst1 lst2)
  (if (and (null? lst1) (null? lst2))
    '()  ; If both lists are empty, return an empty list.
    (cons (+ (first lst1) (first lst2))  ; Add the first elements of the two lists...
          (add-lists (rest lst1) (rest lst2)))))  ; ...then recursively add the rest of the elements.

;; set the memoized strength of each team
(define strength (mem (lambda (player)
  (if (eq? player 'none)
    0
    (abs (gaussian 50 20))))))

;; to evaluate the strength of a team vector, get the strength of the team represented by the vector
(define (team-strength team-vector)
  (sum
    (map (lambda (x) (strength x)) team-vector)
  )
)

;; flip which team wins, winning team gets 3 points in its position in the vector
(define (play-match team-vector-1 team-vector-2)
  (if (flip (/ (team-strength team-vector-1) (+ (team-strength team-vector-1) (team-strength team-vector-2))))
    (map (lambda (x) (if (> x 0) 3 0)) (map (lambda (y) (strength y)) team-vector-1))
    (map (lambda (x) (if (> x 0) 3 0)) (map (lambda (y) (strength y)) team-vector-2))
))

;; recursive function: each list element is a match-up of two team vectors
(define (play-tournament tournament)
  (if (= (length tournament) 1)
    ; down to the last match: just play that match and return the score vector, end the recursion
    (play-match (first (first tournament)) (last (first tournament)))
    ; not yet the last match: play the first match in the list, recurse with the rest of the list, add both score vectors
    (add-lists
      ; play the first match in the list
      (play-match (first (first tournament)) (last (first tournament)))
      ; send the rest of the matches down into the recursion
      (play-tournament (rest tournament))
    )
  )
)

(define (combine-element-with-list element lst)
  (map (lambda (x) (list element x)) lst))

(define (create-tournament teams)
  (if (= (length teams) 2)
    ; down to the last two teams: create a match among them and stop the recursion
    (list (list (first teams) (last teams)))
    ; have the first team in the list play against all remaining teams
    ; then play all matches among teams that don't include the first team
    (append (combine-element-with-list (first teams) (rest teams)) (create-tournament (rest teams)))
  )
)

(define (create-team-vectors teams number)
  (if (= (length teams) 1)
    (list (update-list (make-list number 'none) (- number 1) (first teams)))
    (append (list (update-list (make-list number 'none) (- number (length teams)) (first teams))) (create-team-vectors (rest teams) number))
  )
)

;(let ((teams (list (list 'ger 'none 'none 'none) (list 'none 'ita 'none 'none) (list 'none 'none 'arg 'none) (list 'none 'none 'none 'jpn))))
;  (play-tournament (create-tournament teams))
;)

(let ((teams (create-team-vectors (list 'ger 'fra 'arg 'jpn) 4)))
  (play-tournament (create-tournament teams))
)
