; lib-ac.scm.arc
; by AmkG
; Contains everything that has been 'xdef'ed
; in ac.scm

(= a2c-lib-ac*
'(
;-----------------------------------------------------------------------------
; lib-ac
; The definitions here are the definitions for ac.scm functions accessible
; from arc.
; The following limitations in coding apply:
;   1. No ssyntax.
;   2. 'if forms support only the 2-argument and 3-argument forms, not
;      the full Arc 'if forms.
;   3. No 'def or any other useful macros.
;   4. Use only 'set, not '=.  See #3.
;   5. No 'afn or 'rfn.  See #3.
;   6. Only the single-local-variable 'let and the no-local-variables 'do
;      are supported.
;   7. You better make sure the parens match
;
; For all the above limitations, you do have a few very powerful abilities
; in this part of the code:
;   1. You can access any primitives by prepending them with %.  For
;      example you can access the %car primitive.  If you need to define
;      a primitive, add it to codegen.arc and use it here.
;   2. You can create a lib-ac-only global by prepending it with $.  These
;      globals are inaccessible to plain Arc code, and are accessible only
;      to lib-ac.

; it's okay: the inliner will automatically inline these
; into the user's code.  In addition, the user can still
; access these functions as functions, e.g. for use with
; 'map and other higher-order functions.
(set cons
  (fn (a d) (%cons a d)))

(set car
  (fn (l) (%car l)))

(set cdr
  (fn (l) (%cdr l)))

(set table
  (fn () (%table)))

(set is
  (fn (a b) (%is a b)))

(set isnt
  (fn (a b) (%isnt a b)))

(set $sub-pr
  (fn (rest)
    (if (cdr rest)
        (do
          (%pr (car rest))
          ($sub-pr (cdr rest)))
        (%pr (car rest)))))

(set pr
  (fn rest
    (if rest
        ($sub-pr rest))))

(set $sub-prn
  (fn (rest)
    (if (cdr rest)
        (do
          (%pr (car rest))
          ($sub-prn (cdr rest)))
        (%prn (car rest)))))

(set prn
  (fn rest
    (if rest
        ($sub-prn rest)
        (do (%prn '||)
            nil))))

(set type
  (fn (obj)
    (%type obj)))

(set rep
  (fn (obj)
    (%rep obj)))

(set annotate
  (fn (the-tag obj)
    (%annotate the-tag obj)))

; arguably the "wrong" place to put this in, since
; this is part of arc.arc, but useful anyway
(set list
  (fn rest rest))

;-----------------------------------------------------------------------------
))

(def lib-ac-insert (ast)
  (with (enprim nil
         primvar nil
         privvar nil
         privtb (table))
    (= privvar
       (fn (var)
         (and (aglobal var)
              (is #\$ ((string var!id) 0)))))
    (= primvar
       (fn (var)
         (and (aglobal var)
              (is #\% ((string var!id) 0)))))
    (= enprim
       (fn (ast)
         (if
           ; replace privates with gensyms
           (and ((orf aref aset) ast) (privvar ast!var))
             (if (aref ast)
                 (make-ref '()
                           (or (privtb ast!var)
                               (= (privtb ast!var) (new-global (uniq)))))
                 (make-set (map enprim ast!subx)
                           (or (privtb ast!var)
                               (= (privtb ast!var) (new-global (uniq))))))
           ; replace application of %foo with
           ; primitives
           (and (anapp ast)
                (aref (car ast!subx))
                (primvar ((car ast!subx) 'var)))
             (make-prim (map enprim (cdr ast!subx))
                        (((car ast!subx) 'var) 'id))
           ; this is really beginning to annoy me.
           ; I think the value field of the quote AST
           ; should be put in 'val, not in 'subx
           (aquote ast)
             ast
           ; else
             (do (= ast!subx (map enprim ast!subx))
                 ast))))
    (make-seq
      (join
        (map [enprim:xe _ '()] a2c-lib-ac*)
        (if (aseq ast)
            ast!subx
            (cons ast nil))))))

