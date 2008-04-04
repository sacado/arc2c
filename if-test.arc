
(set test
  (fn (v)
    (if
      (is v 1)
        (prn 'clause1)
      (is v 2)
        (prn 'clause2)
      (is v 'c3)
        (prn 'clause3)
      ; else
        (prn 'final-else-branch))))

(set t2
  (fn (v)
    (if v
        (prn 'then-branch)
        (prn 'else-branch))))

(test 1) ; clause1
(test 2) ; clause2
(test 3) ; final-else-branch
(test 'c3) ; clause3

(t2 t) ; then-branch
(t2 nil) ; else-branch
