; obviously stolen from conanite's arc-in-ciel
(sref call* (fn (the-bobo size) (* the-bobo size)) 'bobo)
; expected output:
; 69
(prn
  ((fn (a-bobo)
     (a-bobo 3))
    (annotate 'bobo 23)))

; expected output:
; ON BOBO: 126
; CAUGHT!
(on-err
  (fn (e)
    (if (is (type e) 'apply)
        (prn "CAUGHT!")
        (err e)))
  (fn ()
    (let f (annotate 'bobo 42)
      (prn "ON BOBO: " (f 3))
      (let v (annotate 'unknown-type 42)
        (prn "ON UNKNOWN-TYPE: " (v 3))))))

