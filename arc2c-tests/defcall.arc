; obviously stolen from conanite's arc-in-ciel
(sref call* (fn (the-bobo size) (* the-bobo size)) 'bobo)
; expected output:
; 69
(prn
  ((fn (a-bobo)
     (a-bobo 3))
    (annotate 'bobo 23)))
