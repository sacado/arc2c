
(let var1 1
  (let var2 2
    (let c (ccc (fn (x) x))
      (prn "var1: " var1)
      (prn "----changing var1:" (set var1 42))
      (prn "var2: " var2)
      (prn "----changing var2:" (set var2 43))
      (if c
          (do (prn "restarting!")
              (c nil))))))
; EXPECTED OUTPUT:
; var1: 1
; ----changing var1:42
; var2: 2
; ----changing var2:43
; restarting!
; var1: 42
; ----changing var1:42
; var2: 43
; ----changing var2:43

