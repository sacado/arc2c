; in-global.arc
; by AmkG
; Trivial global function inlining

(def in-global (ast)
  (with (; internal functions
         is-inlineable nil
         get-inlineable nil
         copy-ast nil
         entable-params-args nil
         perform-inline nil
         ; table of global symbols
         ; for inlining
         inlineable (table)
         ; global symbols which
         ; we have determined to be unsafe
         ; for inlining
         banned (table))
    ; determine if the given ast is indeed an inlineable function.
    (= is-inlineable
       (fn (ast)
         (and
              ; has to be a function or *lam*bda
              (alam ast)
              (with (params (properify ast!params)
                     references (table)
                     check-refs nil)
                ; make sure that parameters
                ; are referenced at most once
                (= check-refs
                   (fn (ast)
                     (if
                       (and (aref ast) (some ast!var params))
                         (if (references ast!var)
                             ; if it's been referenced before,
                             ; fail
                             nil
                             (= (references ast!var) t))
                       ; assignments to a parameter would be
                       ; too complicated for inlining, so fail...
                       (and (aset ast) (some ast!var params))
                         nil
                       ; don't fail quotes
                       (aquote ast)
                         t
                       ; else
                         (all check-refs ast!subx))))
                (check-refs ast)))))
    ; get the set of inlineable functions
    (= get-inlineable
       (fn ()
         (each ast ast!subx
           ; determine if this ast is an assignment
           ; to a global
           (if (and (aset ast) (aglobal ast!var))
               ; determine if the global is not already
               ; in the inlineable set, and if the value
               ; being assigned to it is inlineable
               (if (and (~inlineable ast!var) (is-inlineable (car ast!subx)))
                   (= (inlineable ast!var) (copy-ast (car ast!subx)))
                   ; be paranoid if the global fails to
                   ; pass
                   (= (banned ast!var) t))
               (each b (gv-set ast)
                 (= banned.b t))))
         (each b (keys banned)
           (= inlineable.b nil))))
    (= copy-ast
       (fn (ast)
         (with (subst-copy nil
                replacetb (table))
           (= subst-copy
              (fn (ast)
                (if
                  ; for functions, replace
                  ; the variables
                  (alam ast)
                    (do
                      (each v (properify ast!params)
                        (= replacetb.v (new-var v!id)))
                      (make-lam
                        (map subst-copy ast!subx)
                        (map-improper replacetb ast!params)))
                  (aref ast)
                    (make-ref '()
                              (or (replacetb ast!var) ast!var))
                  (aset ast)
                    (make-set (map subst-copy ast!subx)
                              (or (replacetb ast!var) ast!var))
                  ; else
                    (let rv (table)
                      (ontable k v ast
                        (if (is k 'subx)
                            (= rv.k (map subst-copy v))
                            (= rv.k v)))
                      rv))))
           (subst-copy ast))))
    (= entable-params-args
       (fn (params args)
         (let rv (table)
           ((afn (params args)
              (if
                (no params)
                  (when args
                      (err:tostring
                         (pr "Inline attempt failed, too many arguments: ")
                         (write:source args)))
                (acons params)
                  (if args
                      (with ((param . params) params
                             (arg   . args  ) args)
                        (= rv.param arg)
                        (self params args))
                      (err:tostring
                        (pr "Inline attempt failed, too few arguments: ")
                        (write:source params)))
                ; else, a variadic parameter
                  (= rv.params
                     ((afn (asts)
                        (if asts
                            (make-prim
                               (list (car asts) (self (cdr asts)))
                               '%cons)
                            (make-lit '() nil)))
                      args))))
            params args)
           rv)))
    (= perform-inline
       (fn (ast done)
         ; ast is the AST to process
         ; done is a list of globals that we are currently
         ;  performing inlining on.  We avoid inlining a
         ;  global if we're already inlining it
         (let var nil
           (aif
             (and (anapp ast)
                  (aref (car ast!subx))
                  (= var ((car ast!subx) 'var))
                  (~some var done)
                  inlineable.var)
                ; 'it is now the function that we will inline
                (with (body (perform-inline (copy-ast (car it!subx))
                                            (cons var done))
                       args (map [perform-inline _ done] (cdr ast!subx)))
                  (let paramlookup (entable-params-args it!params args)
                       ((afn (ast)
                          (aif
                             (and (aref ast) (paramlookup ast!var))
                               it
                             (aquote ast)
                               ast
                               (do (= ast!subx (map self ast!subx))
                                   ast)))
                        body)))
             (aquote ast)
                ast
                (do
                  (= ast!subx (map [perform-inline _ done] ast!subx))
                  ast)))))
    ; two passes: first search, then replace
    (get-inlineable)
    (perform-inline ast nil)))

