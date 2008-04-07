
(let x 1
  (set reader
    (fn () x))
  (set writer
    (fn (v) (set x v))))

(set x 42)
(prn (reader))   ; 1
(prn (writer 2)) ; 2
(prn (reader))   ; 2
(prn x)          ; 42

(let x 'hmm
  (let x 'arf
    (prn x) ; arf
    (set x 'grr)
    (prn x) ; grr
  )
  (prn x) ; hmm
)

(let x 1
  (let y 2
    (let z 3
      (prn (+ (+ x y) z)) ; 6
      (set x 42)
      (prn (+ (+ x y) z)) ; 47
    ))
  (prn x) ; 42
)

