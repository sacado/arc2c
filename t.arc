(set fib (fn (n)
  (if (< n 2)
    n
    (+ (fib (-  n 1)) (fib (- n 2))))))


(set list-ref (fn (lst idx)
  (if (is idx 0)
    (car lst)
    (list-ref (cdr lst) (- idx 1)))))


(set no (fn (lst)
  (is lst nil)))


(set len (fn (lst)
  (if (no lst)
    0
    (+ 1 (len (cdr lst))))))


(set range (fn (n)
  (if (is n 0)
    (cons 0 nil)
    (cons n (range (- n 1))))))


(set map (fn (fun lst)
  (if (no lst)
    nil
    (cons (fun (car lst)) (map fun (cdr lst))))))


(set rev (fn (lst)
  (if (no lst)
    nil
    (cons (car lst) (rev (cdr lst))))))


(let lst (range 20)
  (let mapped (map fib lst)
    (prn (type mapped))
    (prn (len mapped))
    (prn mapped)))

(prn 'bye)

