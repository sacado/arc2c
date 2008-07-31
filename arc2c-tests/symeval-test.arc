
(set x 42)

(let x 100
  (prn "x is " x)
  (prn "symeval!x is " symeval!x)
  (let y 'x
    (prn "symeval.y is " symeval.y)))

; expected:
; x is 100
; symeval!x is 42
; symeval.y is 42

