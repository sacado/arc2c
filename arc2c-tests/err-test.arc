
(prn "----on-err catching test")
(prn "result: "
  (on-err
    (fn (e)
      (list "CAUGHT!" e))
    (fn ()
      (pr "pre-error")
      (err "Oops!")
      (pr "post-error"))))
; expected:
; pre-errorresult: (CAUGHT! Oops!)

(prn "----on-err noncatching test")
(prn "result: "
  (on-err
    (fn (e)
      (list "CAUGHT!" e))
    (fn ()
      42)))
; expected:
; result: 42

(err:annotate 'goodbye "Goodbye cruel world!")
(prn "SHOULD NOT RUN!")

