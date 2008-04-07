
(set list
  (fn rest
    rest))

(prn (list 1 2 3)) ; (1 . (2 . (3 . nil)))
(prn (list 'x 'y)) ; (x . (y . nil))
(prn (list)) ; nil

(set test1
  (fn (v . rest)
   (pr 'v) (prn v)
   (pr 'rest) (prn rest)))

; v1
; restnil
(test1 1)
; vx
; rest(y . nil)
(test1 'x 'y)

(set test2
  (fn (x y . rest)
    (pr 'x) (prn x)
    (pr 'y) (prn y)
    (pr 'rest) (prn rest)))

; x1
; y2
; rest(3 . nil)
(test2 1 2 3)
