
(prn "----on-err catching test")
(prn "result: "
  (on-err
    (fn (e)
      (list "CAUGHT!" e))
    (fn ()
      (prn "pre-error")
      (err "Oops!")
      (prn "post-error"))))
; expected:
; pre-error
; result: (CAUGHT! Oops!)

(prn "----on-err noncatching test")
(prn "result: "
  (on-err
    (fn (e)
      (list "CAUGHT!" e))
    (fn ()
      42)))
; expected:
; result: 42

(prn "----on-err nested test catching built-ins")
(prn "result: "
  (on-err
    (fn (e)
      (list "CAUGHT! outer..." (type e) (rep e)))
    (fn ()
      (on-err
        (fn (e)
          (list "CAUGHT!  inner..." (type e) (rep e)))
        (fn ()
          (car "asdf"))))))
; expected:
; result: (CAUGHT!  inner... badargs car expects argument of type 'cons)

(err:annotate 'goodbye "Goodbye cruel world!")
(prn "SHOULD NOT RUN!")

