; rm-global.arc
; by AmkG
; partial dead code elimination - unused global removal
;
; removes assignments to globals, if the global
; is not read.  Also removes bits of the program
; that won't return any values and have side effects,
; such as docstrings.

(def rm-global (ast)
  (with (writeset (gv-set ast)
         readset (gv-read ast)
         ; private functions
         remove-consts nil
         remove-assigns nil)
    ; removes assigments to variables in the
    ; toremove set
    (= remove-assigns
       (fn (toremove ast)
         ; if it's an assignment, and it's one of 
         ; the assigments to remove, then remove the
         ; enclosing assignment: remember that we need
         ; to handle the case:
         ;   (set used-global (set unused-global value))
         ;   =>
         ;   (set used-global value)
         (if
           (and (aset ast) (some ast!var toremove))
             (car ast!subx)
           (aquote ast)
             ast
             (do
               (= ast!subx (map [remove-assigns toremove _] ast!subx))
               ast))))
    ; removes constants and other no-side-effect code
    ; from within sequences
    (= remove-consts
       (fn (ast)
         (prn "remove-consts " (source ast))
         (if
           (aseq ast)
             (do
               (= ast!subx
                  ((afn ((ast . rest))
                     (if
                       rest
                       ; check if ast can't have side-effects
                       ; Note: this test is the most important
                       ; part of the dead-code elimination
                       ; If we can detect that something won't have
                       ; an effect if it isn't the return value,
                       ; then we can eliminate it
                         (if ((orf alit aref aquote alam) ast)
                             (self rest)
                             (cons (remove-consts ast) (self rest)))
                       ; tails are always useful
                       ast
                         (cons (remove-consts ast) nil)))
                   ast!subx))
               ast)
           (aquote ast)
             ast
             (do
               (= ast!subx (map remove-consts ast!subx))
               ast))))
    (aif
      ; remove any global variables written that
      ; aren't read
      (diff writeset readset)
        (rm-global:remove-consts:remove-assigns it ast)
      ; if there's anything read that isn't written,
      ; raise exception
      (diff readset writeset)
        ; allow some undefined globals: these are inserted later in
        ; the transformation
        (if (all [in _!uid 'ccc] it)
          ast
          (err:tostring:pr "Global variables read, but aren't assigned to: "
                           (map [_ 'uid] it)))
      ; else continue
        ast)))

