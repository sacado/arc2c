; CPS conversion

(def make-cont (subx params)
  (let ast (make-lam subx params)
    (assert ast!continuation)
    ast))

(def cps (ast k-ast)
  (if
    (or (alit ast) (aref ast) (aquote ast))
      (make-app (list k-ast ast))
    (aset ast)
      (cps-list ast!subx [make-app:list k-ast (make-set _ ast!var)])
    (acnd ast)
      (let xform (fn (k)
        (cps-list (list:car ast!subx) [make-cnd:list (car _) (cps (cadr ast!subx) k) (cps (car:cddr ast!subx) k)]))
        (if (aref k-ast) ; prevent combinatorial explosion
          (xform k-ast)
          (let k (new-var 'k)
            (make-app:list (make-lam (list:xform:make-ref '() k) (list k)) k-ast))))
    (aprim ast)
      (cps-list ast!subx [make-app:list k-ast (make-prim _ ast!op)])
    (anapp ast)
      (let fun (car ast!subx)
        (if (alam fun)
          ; let form
          (cps-list (cdr ast!subx) [make-app (cons (make-lam (list:cps-seq fun!subx k-ast) fun!params) _)])
          (cps-list ast!subx [make-app (cons (car _) (cons k-ast (cdr _)))])))
    (alam ast)
      (let k (new-var 'k)
        (make-app:list k-ast (make-lam (list:cps-seq ast!subx (make-ref '() k)) (cons k ast!params))))
    (aseq ast)
      (cps-seq ast!subx k-ast)
      (err "unknown ast" ast)))

(def cps-list (asts inner)
  (let body (fn (x) (cps-list (cdr asts) [inner (cons x _)]))
    (if
      (no asts)
        (inner '())
      (or (alit (car asts)) (aref (car asts)))
        (body (car asts))
        (let r (new-var 'r)
          (cps (car asts) (make-cont (list:body:make-ref '() r) (list r)))))))

(def cps-seq (asts k-ast)
  (if
    (no asts)
      (make-app:list k-ast nil)
    (no (cdr asts))
      (cps (car asts) k-ast)
      (let r (new-var 'r)
        (cps (car asts) (make-cont (list:cps-seq (cdr asts) k-ast) (list r))))))


(def cps-convert (ast)
  (with (ast-cps
          (cps
            ast
            (let r (new-var 'r)
              (make-cont
                (list:make-prim (list:make-ref '() r) '%halt)
                (list r))))
         primvar
         (fn (var)
           (and (aglobal var)
                (is #\% ((string var!id) 0))))
         primitivize nil)
    (= primitivize
       (fn (ast)
         (if
           (and (anapp ast)
                (aref:car ast!subx)
                (primvar ((car ast!subx) 'var)))
             (make-prim (map primitivize (cdr ast!subx))
                        (((car ast!subx) 'var) 'id))
           ; else
             (do (zap [map primitivize _] ast!subx)
                 ast))))
    (if (lookup 'ccc (fv ast))
      ; add this definition for call/cc if call/cc is needed
      (= ast-cps
        (make-app:list
          (make-lam (list ast-cps) (list (new-var '_)))
          (xe '(set ccc (fn (k f) (f k (fn (_ result) (k result))))) '()))))
    (if (lookup '<arc2c>!apply (fv ast))
      (= ast-cps
         (make-app:list
          (make-lam (list ast-cps) (list (new-var '_)))
          (primitivize:xe '(set <arc2c>!apply (fn (k f l) (%apply f k l))) '()))))
    ast-cps))

