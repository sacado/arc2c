
(def prsall (rest)
  (if rest
    (do (pr (car rest) #\space)
        (prsall:cdr rest))))

(def map (f l)
  (if l
    (cons (f:car l) (map f (cdr l)))))

(def factorial (n)
  (if (> n 0)
      (* n (factorial (- n 1)))
      1))

(def choose (n k)
  ; arc2c only supports two-arg forms for math
  (/ (/ (factorial n) (factorial k))
     (factorial (- n k))))

(def indices (n)
  (let self nil
    ; no support for '= yet
    (set self
      (fn (i)
        (if (isnt i n)
            (cons i (self (+ i 1))))))
    (self 0)))

(def pascal-triangle (n)
  (let self nil
    (set self
      (fn (i)
        (if (<= i n)
            (do
              ; Anarki make-br-fn not supported yet
              (prsall (map (fn (_) (mod (choose i _) 2)) (indices (+ i 1))))
              (prn)
              (self (+ i 1))))))
    (self 0)))

(pascal-triangle 4)
