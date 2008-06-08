
(set multiple
  (fn (a b c)
    (prn "----multiple")
    (prn "a " a)
    (prn "b " b)
    (prn "c " c)))

(apply multiple '(1 2 3))
; expected:
; ----multiple
; a 1
; b 2
; c 3

(on-err
  (fn (e)
    (if (is (type e) 'apply)
        (prn "CAUGHT: " e)
        (err e)))
  (fn ()
    (apply multiple '(1 2))))
; expected:
; CAUGHT: #3(tagged apply "Expected blah blah")

(apply prn (list 5 6 7 8))
; expected:
; 5678

