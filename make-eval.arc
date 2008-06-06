; make-eval.arc
; by AmkG
; An evaller for macros

; notes:
; 1. the created 'eval simulates a global environment
;    based on the current Arc environment, but attempts
;    to prevent the current Arc environment from being
;    mutated by interpreted code.  Basically any globals
;    read will be copied to a table shadowing the globals
;    - if a global is referenced by 'evaled code, it is
;    first searched in the shadow table
; 2. we *create* a new eval for each run.  Since this
;    eval actually keeps a table which shadows globals,
;    this prevents previous compilation runs from
;    impacting the current run.
; The above are necessary to prevent macro code from
; overriding the compiler code; we can't directly use
; 'eval ^^

(def make-eval ()
  " Creates an 'eval-like function for macros. "
  (withs (global-env (table)
          deep-copy nil
          symeval
            (rfn symeval (e (o env global-env))
              (unless (isa e 'sym)
                (err:tostring:pr "make-eval/symeval: not a symbol - " e))
              (if env
                (aif (env e)
                     it
                     (symeval e (env "parent")))
                ; protect the global: make a copy of
                ; it, don't let the target mutate
                ; it directly.
                (= global-env.e (deep-copy:eval e))))
          macro-expand
            (fn (macname . args)
              (let macfn (symeval macname)
                (apply (rep macfn) args))))
    (= deep-copy
       (fn (o)
         (if
           ; annotated object
           (isnt (rep o) o)
             (annotate (deep-copy:type o) (deep-copy:rep o))
           ; list
           (acons o)
             (cons (deep-copy:car o) (deep-copy:cdr o))
           ; string
           (isa o 'string)
             (copy o)
           ; table
           (isa o 'table)
             (let rv (table)
               (ontable k v o
                 (= rv.k (deep-copy v)))
               rv)
           ; thread-local
           (isa o 'thread-local)
             (let rv (thread-local)
               (= (rv) (o))
               rv)
           ; anything else
             o)))
    ; overload 'type to convert 'make-eval-interpreted-fn to 'fn
    (= global-env!type
       (fn (o)
         (let typ (type o)
           (if (is typ 'make-eval-interpreted-fn)
               'fn
               typ))))
    ; the eval function
    (afn (e (o env global-env))
      (zap remove-ssyntaxes e)
      (if
        (isa e 'sym)
          (symeval e env)
        (isa e 'cons)
          (if (and (isa (car e) 'sym) (isa (symeval:car e) 'mac))
              ; macro expand
              (self:macro-expand e)
              ; check the expression
              (case (car e)
                ; TODO: special handling for:
                ; quote quasiquote if set lset
                ; (arc does *not* special-case anything else)
                fn
                  (annotate 'make-eval-interpreted-fn
                            (table 'params (cadr e)
                                   'code (cddr e)
                                   'closure env
                                   'evaller self))
                ; *everything* else is a function apply
                  (apply (self:car e) (map self e))))
          ; everything else evals to itself
          e))))

; to allow "real" functions to call 'make-eval-interpreted-fn
(defcall make-eval-interpreted-fn  (f . args)
  (with (f (rep f)
         new-env (table)
         rv nil
         destructure
           (afn () ()))
    ; assign arguments to parameter variables
    ((afn (params args)
       (if
         (no params)
           (if args (err "make-eval fn object: Too many arguments"))
         (acons params)
           ; TODO: destructure function
           (if (acons:car params)
               (destructure new-env params args)
               (do (= (new-env:car params) (car args))
                   (self (cdr params) (cdr args))))
         ; a "rest" argument
           (= new-env.params args)))
     f!params args)
    (= (new-env "parent") f!closure)
    (each e f!code
      (= rv (f!evaller e new-env)))
    rv))

