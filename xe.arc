; xe = expand expression     xe :: (expr,cte) -> ast

(def xe (e cte)
  (if
    (const-expr? e)
      (xe-const-expr e cte)
    (ident-expr? e)
      (xe-ident-expr e cte)
    (form-expr? e)
      (xe-form-expr e cte)
      (err "syntax-error" e)))

(def const-expr? (e)
  (or (in e t nil) (in (type e) 'int 'num 'char 'string)))

(def ident-expr? (e)
  (isa e 'sym))

(def form-expr? (e)
  (and e (alist e))) ; a non-empty list

(def xe-const-expr (e cte)
  (make-lit '() e))

(def xe-ident-expr (e cte)
  (let b (xe-lookup e cte)
    (if (avar b)
      (make-ref '() b)
      (err "can't reference a nonvariable" e))))

(def xe-form-expr (e cte)
  (let h (car e)
    (let b (and (ident-expr? h) (xe-lookup h cte))
      (if (amacro b)
        (b!expander e cte)
        (make-app (xe-exprs e cte))))))

(def xe-exprs (le cte)
  (map [xe _ cte] le))

(= initial-ctes* '())

(mac add-macro (name nbargs)
  (w/uniq prim-name
    `(let ,prim-name (coerce (+ "%" (coerce ',name 'string)) 'sym)
      (push (make-macro ',name
        (fn (e cte)
          (if (is (len (cdr e)) ,nbargs)
            (make-prim (xe-exprs (cdr e) cte) ,prim-name)
            (err:string ',name " : expects " ,nbargs "arg(s)")))) initial-ctes*))))

(add-macro < 2)
(add-macro > 2)
(add-macro <= 2)
(add-macro >= 2)
(add-macro + 2)
(add-macro - 2)
(add-macro * 2)
(add-macro / 2)
(add-macro mod 2)

(push (make-macro 'do
  (fn (e cte)
    (if
      (is (len (cdr e)) 0)
        (xe nil cte)
      (is (len (cdr e)) 1)
        (xe (cadr e) cte)
        (make-seq (xe-exprs (cdr e) cte))))) initial-ctes*)

(push (make-macro 'let
  (fn (e cte)
    (if (>= (len (cdr e)) 1)
      (xe (list (+ (list 'fn (list e.1)) (cut e 3)) e.2) cte)
      (err "let expects a binding")))) initial-ctes*)

(push (make-macro 'or
  (fn (e cte)
    (if
      (is (len (cdr e)) 0)
        (xe nil cte)
      (is (len (cdr e)) 1)
        (xe (cadr e) cte)
        (xe `((lambda (t1 t2) (if t1 t1 (t2))) ,(cadr e) (lambda () (or ,@(cddr e)))) cte)))) initial-ctes*)

(push (make-macro 'and
  (fn (e cte)
    (if
      (is (len (cdr e)) 0)
        (xe t cte)
      (is (len (cdr e)) 1)
        (xe (cadr e) cte)
        (xe `((lambda (t1 t2) (if t1 (t2) t1)) ,(cadr e) (lambda () (and ,@(cddr e)))) cte)))) initial-ctes*)

(push (make-macro '=
  (fn (e cte)
    (xe (cons 'set (cdr e)) cte))) initial-ctes*)

(push (make-macro 'def
  (fn (e cte)
    (xe `(set ,(cadr e) (fn ,(car:cddr e) ,@(cdr:cddr e))) cte))) initial-ctes*)

;-----------------------------------------------------------------------------
; all the macros above this line should eventually be ported
; into lib-ac.scm.arc or in our version of arc.arc

(push (make-macro 'quote
  (fn (e cte)
    (if (is (len (cdr e)) 1)
      (make-quote '() (cadr e))
      (err "quote expects 1 arg")))) initial-ctes*)

(push (make-macro 'set
  (fn (e cte)
    (if (is (len (cdr e)) 2)
      (let b (xe-lookup (cadr e) cte)
        (if (avar b)
          (make-set (xe-exprs (cddr e) cte) b)
          (err "can't set a nonvariable" e)))
      (err "set expects 2 args")))) initial-ctes*)

(push (make-macro 'if
  (fn (e cte)
    (if
      (is (len (cdr e)) 3)
        (make-cnd (xe-exprs (cdr e) cte))
      (is (len (cdr e)) 2)
        (xe `(if ,(cadr e) ,(car:cddr e) nil) cte)
        (err "if expects 2 or 3 args")))) initial-ctes*)

(push (make-macro 'fn
  (fn (e cte)
    (let (_ exp-params . body) e
      (if (>= (len (cdr e)) 1)
        (withs
          ; support proper and improper lists
          ; as argument list: (arg arg) or (arg . arg)
          (params (map-improper new-var exp-params)
           proper-params (if (alist params) (makeproper params) (cons params nil))
           new-cte (extend proper-params cte))
          (make-lam (list:xe (cons 'do body) new-cte) params))
        (err "fn expects a parameter list"))))) initial-ctes*)

(push (make-macro 'symeval
  (fn (e cte)
    (if (isnt 2 (len e)) (err "symeval expects 1 arg"))
    (let (_ form) e
      (if (caris form 'quote)
          (make-ref '() (xe-lookup (cadr form) '()))
          (make-prim (list:xe form cte) '%symeval))))) initial-ctes*)

(def make-initial-cte ()
  initial-ctes*)

(def xe-lookup (id cte)
  (or
    (lookup id cte)
    (lookup id xe-global-cte*)
    (let v (new-global id)
      (push v xe-global-cte*)
      v)))

(= xe-global-cte* (make-initial-cte))

(def parse-file (filename)
  (= xe-global-cte* (make-initial-cte))
  (xe
    (w/infile f filename
      (cons 'do (readall f
                         (list 'used-to-detect-eof))))
    '()))

