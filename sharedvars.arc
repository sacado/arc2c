; sharedvars.arc
; by AmkG
; Shared variables transformation:
;
; (fn (x)
;   (list
;     (fn () x)
;     (fn (v) (set x v))))
;
; =>
;
; (fn (x)
;   ((fn (shared)
;     (list
;       (fn () (%sharedvar-read shared))
;       (fn (v) (%sharedvar-write shared v))))
;    (%sharedvar x)))
;
; Based on an idea by stefano

(def sharedvars-convert (ast)
  (with (sc [map sharedvars-convert _]
         ; flip the arguments to make-lam
         make-fn (fn (x y) (make-lam y x))
         transform nil
         transform-shared nil)
    ; creates a function whose shared variables
    ; have been transformed to explicit objects
    (= transform
       (fn (params subx shared)
         (with (lookup (table))
           (each v shared
             (= lookup.v (new-var 'shared)))
           ; fn ,params
           (make-fn params
             ; apply
             (list:make-app
               (cons
                 ; fn shared
                 (make-fn (map [lookup _] shared)
                   ; ,@body
                   (transform-shared subx lookup))
                 ; (map [%sharedvar _] shared)
                 (map
                   [make-prim (list:make-ref () _) '%sharedvar]
                   shared) ))))))
    ; transform references to shared variables
    (= transform-shared
       (fn (subx lookup)
         (mapeach ast subx
           (if
             ((orf alit aquote) ast)
               ast
             (aref ast)
               (aif (lookup ast!var)
                    (make-prim
                      (list:make-ref () it)
                      '%sharedvar-read)
                    ast)
             (aset ast)
               (aif (lookup ast!var)
                 (make-prim
                   (cons (make-ref () it)
                         ; we depend on 'xe to ensure that
                         ; 'set only has a singleton list in
                         ; its subx
                         (transform-shared ast!subx lookup))
                   '%sharedvar-write)
                 (make-set (transform-shared ast!subx lookup) ast!var))
             (aquote ast)
               ast
             ; else
               (do
                 (= ast!subx (transform-shared ast!subx lookup))
                 ast)))))
    (if
      ((orf alit aref aquote) ast)
        ast
      (aset ast)
        (make-set (sc ast!subx) ast!var)
      (acnd ast)
        (make-cnd (sc ast!subx))
      (aprim ast)
        (make-prim (sc ast!subx) ast!op)
      (anapp ast)
        (make-app (sc ast!subx))
      (alam ast)
        (with (params ast!params
               ; TODO: shared should only have local variables that are:
               ; 1. assigned anywhere, and
               ; 2. are used in more than one sub-lambda
               ; Note however that we then have to handle assignments
               ; to locals in the codegen
               shared (lv-set ast)
               proper-params)
          (= proper-params (properify params))
          (if (some [some _ proper-params] shared)
              (transform params
                         (sc ast!subx)
                         (keep [some _ proper-params] shared))
              (make-lam (sc ast!subx) params)))
      (aseq ast)
        (make-seq (sc ast!subx)))))

(def sharedvars-convert-assert (ast)
  (let rv (sharedvars-convert ast)
    (awhen (lv-set rv)
      (ppr-sexp:source rv)
      (err:tostring:pr
        "sharedvar-convert assertion failed: local variables set - " it))
    rv))

