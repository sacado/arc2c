(set aha '(a b c d))
(prn aha)

(set htest (table))
(prn:type htest)
(sref htest 'foo 3)
(sref htest 'bar 3)
(sref htest 'foo t)
(prn htest)
(prn (htest 3))

(prn (- 4.2 3.1))
(prn (- 12.34567 5))

(def fib (n)
  "The stupid fib implementation"
  (if (> 2 n)
    n
    (+ (fib (-  n 1)) (fib (- n 2)))))

(def ffact (n)
  "Factorial, but with float numbers to see if we can handle them well"
  (if (<= n 1.0)
    1.0
    (* n (ffact (- n 1.0)))))

(def no (lst) (is lst nil))

(def Range (n)
  (if (is n 0)
    '()
    (cons n (Range (- n 1)))))

(set Map (fn (fun lst)
	(if (no lst)
		nil
		(cons (fun (car lst)) (Map fun (cdr lst))))))

(set Rev (fn (lst)
	(if no.lst
		nil
		(cons (car lst) (Rev (cdr lst))))))
(prn aha)

(let lst (Range 30)
	(let mapped (Map (fn (n) (fib n)) lst)
		(prn:type mapped)
		(prn:len mapped)
		(prn:Rev mapped)))
(prn aha)

(set utf8-test "UTF-8 test : $£³µù§ø&éèçàÉÀÈÙÔÜ")

(prn:rep "I'm alive !!!")
(prn:len:rep "I'm alive !!!")


(prn utf8-test)
(sref utf8-test #\u1234 1)
(prn utf8-test)
(prn "The changed character was " #\u1234)
(prn utf8-test.0 utf8-test.1 (utf8-test 2))

(prn (ffact 10.0))

(prn 'foo)
(set foo (annotate 3 'foo))
(prn (is type.foo 3))
(prn:rep foo)
(prn:type foo)

(set l (fn a (prn a) (prn 'hou)))
(prn (l 1))
(prn "tatot")
(prn '(foo bar bye))
(prn ('(foo bar bye) 2))
(prn aha)
(prn #\a)

(prn (isnt 'foo 'foo))
(prn (is '(foo) '(foo)))
(prn (is "bar" "baz"))
(prn (isnt "bar" "bar"))

