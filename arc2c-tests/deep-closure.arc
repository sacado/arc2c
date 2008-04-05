
(set curried
  (fn (w)
    (fn (x)
      (fn (y)
        (fn (z)
          (prn w)
          (prn x)
          (prn y)
          (prn z)
          (prn '----)
          (prn (+ (+ (+ w x) y) z)))))))

((((curried 1) 2) 3) 4)
(prn '||)
((((curried 5) 6) 7) 8)

