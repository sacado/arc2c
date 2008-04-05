; expected output:
; 1
; 1
; 2

(let k (ccc (fn (k) k))
  (prn 1)
  (ccc (fn (k2) (k k2)))
  (prn 2))

