; structs.arc
; by sacado
; comments by AmkG
; Contains definitions for the abstract syntax trees used
; in CPS- and closure-conversions.

; a variable
(def make-var (id uid)
  (listtab `((type var) (id ,id) (uid ,uid))))

(def avar (x)
  (and (isa x 'table) (is x!type 'var)))

; a special form (not really a macro, at least not
; an Arc-style macro, since the output of the macro
; should be an AST, not a list)
(def make-macro (id expander)
  (listtab `((type macro) (id ,id) (expander ,expander))))

(def amacro (x)
  (and (isa x 'table) (is x!type 'macro)))

;----------------------------------------AST's

; a literal: number or t/nil
; (might be extended to chars someday)
(def make-lit (subx val)
  (listtab `((type lit) (subx ,subx) (val ,val))))

(def alit (x)
  (and (isa x 'table) (is x!type 'lit)))

; a variable reference
(def make-ref (subx var)
  (listtab `((type ref) (subx ,subx) (var ,var))))

(def aref (x)
  (and (isa x 'table) (is x!type 'ref)))

; a quoted form
; (currently only literals)
(def make-quote (subx val)
  (listtab `((type quote) (subx ,subx) (val ,val))))

(def aquote (x)
  (and (isa x 'table) (is x!type 'quote)))

; a (set var val) form
(def make-set (subx var)
  (listtab `((type set) (subx ,subx) (var ,var))))

(def aset (x)
  (and (isa x 'table) (is x!type 'set)))

; an if form
; (supports only 2 or 3-arg form but code
; is preprocessed to that form anyway)
(def make-cnd (subx)
  (listtab `((type cnd) (subx ,subx))))

(def acnd (x)
  (and (isa x 'table) (is x!type 'cnd)))

; a primitive
; (possibly make these *optimized* primitives,
; since it's technically possible, in Arc, to
; redefine most primitives; basically if a
; primitive is redefined, it's no longer
; represented using this)
(def make-prim (subx op)
  (listtab `((type prim) (subx ,subx) (op ,op))))

(def aprim (x)
  (and (isa x 'table) (is x!type 'prim)))

; a function call (a function *app*lication)
(def make-app (subx)
  (listtab `((type app) (subx ,subx))))

(def anapp (x)
  (and (isa x 'table) (is x!type 'app)))

; a function form (a *lam*bda)
(def make-lam (subx params)
  (listtab `((type lam) (subx ,subx) (params ,params))))

(def alam (x)
  (and (isa x 'table) (is x!type 'lam)))

; a do form
(def make-seq (subx)
  (listtab `((type seq) (subx ,subx))))

(def aseq (x)
  (and (isa x 'table) (is x!type 'seq)))


(def extend (bindings env)
  (+ bindings env))

(def lookup (id env)
  (if (no env)
    nil
    (let head (car env)
      (if (is head!id id)
        head
        (lookup id (cdr env))))))

(= seq-num* 0)

(def new-var (id)
  (++ seq-num*)
  (make-var id (sym (string id "@" seq-num*))))

(def new-global (id)
  (make-var id id))

(def aglobal (var)
  (is var!id var!uid))

