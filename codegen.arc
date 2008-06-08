;------------------------------------------------------------------------------

; code generation

(def code-generate (ast)
  (withs
    (lambda-todo '()
     lambda-count 0
     global-vars (fv ast)
     add-lambda [do1 lambda-count (push (cons lambda-count _) lambda-todo) (++ lambda-count)]
     constant-todo '()
     constant-count 0
     add-constant [do1 constant-count
                       (push _ constant-todo)
                       (++ constant-count)]
     code-gen nil
     compile-all-lambdas nil
     compile-constants nil)

  ; make sure global-vars includes call*
  (unless (some [is _!id _!uid 'call*]
                global-vars)
    (push (new-global 'call*) global-vars))

  (= code-gen (fn (ast stack-env)
    (let (cg-list cg-args access-var cg) nil
      (= cg-list (fn (asts vars stack-env sep k)
        (if (no asts)
          (k "" stack-env)
          (let x (code-gen (car asts) stack-env)
            (cg-list (cdr asts) (cdr vars) (cons (car vars) stack-env) sep (fn (code stack-env) (k (list x sep code) stack-env)))))))

      (= cg-args (fn (args stack-env)
        (cg-list args (if args (range 1 (len args))) stack-env "" (fn (code stack-env) code))))

      (= access-var (fn (var stack-env)
        (if (aglobal var)
          (let i (pos var global-vars)
            (list "GLOBAL(" i "/*" var!uid "*/)"))
          (let i (- (len stack-env) (pos var stack-env) 1)
            (list "LOCAL(" i "/*" var!uid "*/)")))))

      (= cg (fn (ast)
        (if
          ; TODO: make aquote and alit the same!
          (alit ast)
            (let val ast!val
              (if
                (no val) (list " PUSH(NILOBJ);")
                (is val t) (list " PUSH(TOBJ);")
                (isa val 'char) (list " PUSH(CHAR2OBJ(" coerce.val!int "));")
                ; NONCANONICAL!
                ; (def foo ()
                ;   "string")
                ; (sref (foo) #\x 0)
                ; (prn (foo))
                ; alit AST's should probably be stored in QUOTE_CONSTANTS
                ; too
                (isa val 'string) (list " PUSH((obj)utf82str(\"" val "\"));")
                (isa val 'num) (list " PUSH((obj)DBL2OBJ(" val "));")
                (list " PUSH(FIX2OBJ(" (coerce (num val) 'int) "));"))) ; Just be sure it is an actual int, not something like 1.0
          (aquote ast)
            (let val ast!val
              (list " PUSH(QUOTE_CONSTANTS[" (add-constant val) "]"
                    "/* '" (tostring:write val) " */);"))
          (aref ast)
            (list " PUSH(" (access-var ast!var stack-env) ");")
          (aset ast)
            (list (cg (car ast!subx)) " " (access-var ast!var stack-env) " = TOS();")
          (acnd ast)
            (let x (map cg ast!subx)
              (list (car x) "\n if (POP() != NILOBJ) {\n" (cadr x) "\n } else {\n" (car:cddr x) "\n }"))
          (aprim ast)
            (let args ast!subx
              (if
                (is ast!op '%apply)
                  (list (cg-args args stack-env) " APPLY();")
                (is ast!op '%string-ref)
                  (list (cg-args args stack-env) " STRING_REF();")
                (is ast!op '%list-ref)
                  (list (cg-args args stack-env) " LIST_REF();")
                (is ast!op '%table-ref)
                  (list (cg-args args stack-env) " TABLE_REF();")
                (is ast!op '%set-err)
                  (list (cg-args args stack-env) " SET_ERR();")
                (is ast!op '%curr-err)
                  (list (cg-args args stack-env) " CURR_ERR();")
                (is ast!op '%sharedvar)
                  (list (cg-args args stack-env) " MAKE_SHAREDVAR();")
                (is ast!op '%sharedvar-read)
                  (list (cg-args args stack-env) " READ_SHAREDVAR();")
                (is ast!op '%sharedvar-write)
                  (list (cg-args args stack-env) " WRITE_SHAREDVAR();")
                (is ast!op '%sref) (list (cg-args args stack-env) " SREF();")
                (is ast!op '%cons) (list (cg-args args stack-env) " CONS();")
                (is ast!op '%car) (list (cg-args args stack-env) " CAR();") 
                (is ast!op '%cdr) (list (cg-args args stack-env) " CDR();") 
                (is ast!op '%len) (list (cg-args args stack-env) " LEN();") 
                (is ast!op '%type) (list (cg-args args stack-env) " TYPE();")
                (is ast!op '%annotate) (list (cg-args args stack-env) " ANNOTATE();")
                (is ast!op '%rep) (list (cg-args args stack-env) " REP();")
                (is ast!op '%is) (list (cg-args args stack-env) " EQ();")
                (is ast!op '%isnt) (list (cg-args args stack-env) " NEQ();")
                (is ast!op '%<) (list (cg-args args stack-env) " LT();")
                (is ast!op '%>) (list (cg-args args stack-env) " GT();")
                (is ast!op '%<=) (list (cg-args args stack-env) " LE();")
                (is ast!op '%>=) (list (cg-args args stack-env) " GE();")
                (is ast!op '%+) (list (cg-args args stack-env) " ADD();")
                (is ast!op '%-) (list (cg-args args stack-env) " SUB();")
                (is ast!op '%*) (list (cg-args args stack-env) " MUL();")
                (is ast!op '%/) (list (cg-args args stack-env) " DIV();")
                (is ast!op '%mod) (list (cg-args args stack-env) " MOD();")
                (is ast!op '%pr) (list (cg-args args stack-env) " PR();") 
                (is ast!op '%prn) (list (cg-args args stack-env) " PRN();")
                (is ast!op '%table) (list (cg-args args stack-env) " TBL();")
                (is ast!op '%halt) (list (cg-args args stack-env) " HALT();")
                (is ast!op '%closure)
                  (withs
                    (i (add-lambda (car args))
                     n (len (cdr args))
                     s (list "CLOSURE(" i "," n ");"))
                    (list (cg-args (cdr args) stack-env) " BEGIN_" s (map [list " INICLO(" _ ");"] (if (isnt n 0) (rev:range 1 n))) " END_" s))
                (is ast!op '%closure-ref)
                  (let i ((cadr args) 'val)
                    (list (cg (car args)) " TOS() = CLOSURE_REF(TOS()," i ");"))
                (err:string "unknown primitive" ast!op)))
          (anapp ast)
            (withs
              (fun (car ast!subx)
               args (cdr ast!subx)
               n (len args))
              (if (alam fun)
                (cg-list args (properify fun!params) stack-env "\n" (fn (code new-stack-env) (list code (code-gen (car fun!subx) new-stack-env))))
                (cg-list args (if (isnt 0 n) (range 1 n)) stack-env "\n" (fn (code new-stack-env)
                  (with
                    (start (len stack-env)
                     s (list "JUMP(" n ");"))
                    (list code " BEGIN_" s (map [list " PUSH(LOCAL(" (+ _ start) "));"] (if (isnt n 0) (range 0 (- n 1)))) " END_" s))))))
          (alam ast) ; this case is impossible after CPS-conversion
            (list " PUSH(FIX2OBJ(" (add-lambda ast) "));")
          (aseq ast) ; this case is impossible after CPS-conversion
            (map [list (cg _) "DROP();"] ast!subx)
            (err "unknown ast" ast))))

      (cg ast))))

  (= compile-all-lambdas (fn ()
    (if (no lambda-todo)
      ""
      (withs
        (x (car lambda-todo)
         ast (cdr x))
        (pop lambda-todo)
          (list
            "case " (car x) ":\n\n"
            ; determine the number of required arguments
            (let num-reqs (len:properify ast!params)
              ; if variadic, use variadic handling code
              (if (dotted ast!params)
                  ; the second argument is the arc-side expected number of
                  ; arguments (arguments include the function to execute as
                  ; well as the continuation, which the arc-side programmer
                  ; doesn't care about)
                  (list " VARIADIC2LIST(" (- num-reqs 1) "," (- num-reqs 3) ");\n")
                  (list " CHECK_PARAMS(" num-reqs "," (- num-reqs 2) ");\n")))
            (code-gen (car ast!subx) (rev:properify ast!params))
            "\n\n"
            (compile-all-lambdas))))))

  (= compile-constants
    (fn (cs)
      (with (i 0
             express nil)
        (= express
           (fn (e)
             (if
               (isa e 'int) (list "PUSH(FIX2OBJ(" e "));\n")
               (no e)       "PUSH(SYM2OBJ(\"nil\"));\n"
               (isa e 'sym) (list "PUSH(SYM2OBJ(" (tostring:write:string e)
                                  "));\n")
               (acons e)
                 (list (express:car e) (express:cdr e) "CONS();\n")
               ;else
                 (err:string "not supported in quote form: "
                             (tostring:write e)))))
        (mapeach c cs
          (do1
            (list (express c) "QUOTE_CONSTANTS[" i "] = POP();\n")
            (++ i))))))

  (add-lambda ast)

  (let code (compile-all-lambdas)
    (list
      (list "#define NB_GLOBALS " (len global-vars) "\n"
            "#define MAX_STACK " 2000 "\n"
            "#define NB_QUOTE_CONSTANTS " constant-count "\n"
            "#define CALL_STAR GLOBAL("
              (pos [is _!uid _!id 'call*] global-vars)
            ")\n")
      code-header*
      (if constant-todo
        ; constants were defined; initialize
        (list "void init_constants(void){\nsp = stack;\n"
              (compile-constants:rev constant-todo)
              "}\n")
        ; no constants; dummy initialize
        "\n#define init_constants()\n")
      code-prefix*
      code-execute*
      code
      code-suffix*))))


(def readtxtfile (name)
  (w/infile f name
    (let res ""
      (whilet line (readline f) (++ res (string line "\n")))
      res)))


(= code-header* (readtxtfile "a2c.h")  ; typedefs, function headers & macros
   code-prefix* (readtxtfile "a2c.c")) ; predefined functions

(= code-execute* "

obj execute (int pc)
{
  sp         = stack;
  obj NILOBJ = SYM2OBJ (\"nil\");
  obj TOBJ   = SYM2OBJ (\"t\");
  obj errhandler; /*TODO: make this thread-local*/
  long num_args = 0;

  jump: switch (pc) {

")

(= code-suffix* "  }
  return POP();
}
")

;------------------------------------------------------------------------------


; debugging

(def source (ast)
  (if
    (alit ast)
       ast!val
    (aref ast)
      ((ast 'var) 'uid)
    (aset ast)
      (list 'set ((ast 'var) 'uid) (source (car ast!subx)))
    (acnd ast)
      (cons 'if (map source ast!subx))
    (aprim ast)
      (cons ast!op (map source ast!subx))
    (anapp ast)
      (if (alam (car ast!subx))
        ; actually, shouldn't properify the params, maybe okay since this is debug code, but...
        (list 'let (map (fn (p a) (list p!uid (source a))) (properify ((car ast!subx) 'params)) (cdr ast!subx)) (source (car ((car ast!subx) 'subx))))
        (map source ast!subx))
    (alam ast)
      (list 'fn (map-improper [_ 'uid] ast!params) (source (car ast!subx)))
    (aseq ast)
      (cons 'do (map source ast!subx))
    (aquote ast)
      (list 'quote ast!val)
      ; if unknown AST, then probably still in list form
      ; (cref driver)
      ast))

; seems currently unused -
; note that this function doesn't support varargs
(def ds (ast)
   (if
      (alit ast)
          (cons 'lit ast!val)
      (aref ast)
         (cons 'ref ((ast 'var) 'uid))
      (aset ast)
         (cons 'set (list 'set ((ast 'var) 'uid) (ds (car ast!subx))))
      (acnd ast)
         (cons 'cnd (cons 'if (map ds ast!subx)))
      (aprim ast)
         (cons 'prim (cons ast!op (map ds ast!subx)))
      (anapp ast)
      (cons 'app
         (if (alam (car ast!subx))
            (list 'let (map (fn (p a) (list p!uid (ds a))) ((car ast!subx) 'params) (cdr ast!subx)) (ds (car ((car ast!subx) 'subx))))
            (map ds ast!subx)))
      (alam ast)
         (cons 'lam (list 'fn (map [_ 'uid] ast!params) (ds (car ast!subx))))
      (aseq ast)
         (cons 'seq (cons 'do (map ds ast!subx)))
         ast))

; abstract away avoiding ' and the quoted parts of `
; body *must* return the value in 'var if it won't
; change anything
(mac code-walk (var code . body)
  (w/uniq usercode
    `(let ,usercode (fn (,var) ,@body)
       (*code-walk-internal ,usercode ,code))))

(def *code-walk-internal (usercode code)
  (let pass-to-user
       (fn (c)
         (let new-code (usercode c)
           (if (isnt new-code c)
               ; if change, rewalk the new code
               ; (don't move on unless code is stable)
               (*code-walk-internal usercode new-code)
               (if (acons new-code)
                   (map-improper [*code-walk-internal usercode _] new-code)
                   new-code))))
    (if (acons code)
        (if
          (is (car code) 'quote)
            code
          (is (car code) 'quasiquote)
            (list 'quasiquote
               ((rfn quasiwalk (code)
                   (if (acons code)
                       (if
                         (is (car code) 'unquote)
                           (list 'unquote (*code-walk-internal usercode
                                                               (cadr code)))
                         (is (car code) 'unquote-splicing)
                           (list 'unquote (*code-walk-internal usercode
                                                               (cadr code)))
                         ; else
                           (map-improper quasiwalk code))
                       code))
                 (cadr code)))
          ; else
            (pass-to-user code))
        (pass-to-user code))))

(def to-3-if (l)
  (code-walk code l
    (if
      (and (caris code 'if) (len> code 4))
        (let (_ c v . rest) code
          `(if ,c ,v (if ,@rest)))
      ; else
        code)))

(def symbol-syntax (l)
  (code-walk code l
    (remove-ssyntaxes code)))

; When self-compiling, probably need to define
; ssyntax and ssexpand lib functions
(def remove-ssyntaxes (code)
  (if
    (and (acons code) (ssyntax:car code))
      (let (fun . args) code
        (zap ssexpand fun)
        (if (caris fun 'compose)
            ((afn ((fun . rest))
               (if rest
                   `(,fun ,(self rest))
                   `(,fun ,@args)))
             (cdr fun))
            `(,fun ,@args)))
    (ssyntax code)
      (ssexpand code)
    code))

; handles (toplevel) 'load and 'require
; inefficiency note: allocs a lot of cons cells
(def include-files (l)
  (mappend
    (fn (e)
      ; if it's a 'load or 'require, include it
      (if (and (acons e) (in (car e) 'load 'require))
          (let (_ file . oops) e
            (when oops
              (err:string "Too many parameters to: " (car e)))
            ; make sure to recursively handle including files
            (include-files
              (w/infile s file
                (readall s (list 'detect-eof)))))
          (list e)))
    l))

;------------------------------------------------------------------------------

(def strip-ext (filename)
  (cut filename 0 (pos #\. filename)))

(def compile-file (filename (o debugmode t))
  (with (d (w/infile s filename (cons 'do (readall s (list 'detect-eof))))
         chain
         `(
            ; --------List form
            (,include-files "FILE INCLUSION")
            ; NOTE: all the 'code-walk passes might be
            ; better off in a single pass (just use a large
            ; 'if block).  Especially with regards to macros
            ; and symbol syntax: a ssyntax might emit a macro,
            ; which might emit a ssyntax, which might emit
            ; a macro... 'code-walk can actually handle this
            ; but only if macros and ssyntax expansion are
            ; in the same 'code-walk block
            (,symbol-syntax "ssyntax TRANSFORMATION")
            (,to-3-if "3-arg-if TRANSFORMATION")
            ; --------AST form
            (,[xe _ ()] "AST TRANSFORMATION")
            (,lib-ac-insert "ac.scm LIBRARY INSERTION")
            (,in-global "GLOBAL INLINING")
            (,rm-global "UNUSED GLOBAL REMOVAL")
            (,sharedvars-convert-assert "SHARED VARIABLE CONVERSION")
            (,cps-convert "CPS-CONVERSION")
            (,closure-convert "CLOSURE-CONVERSION")))
    (= xe-global-cte* (make-initial-cte))
    (when debugmode
      (w/outfile stream "arc2c.log"
        nil))
    (= d
      (reduce (fn (old-d (f desc))
                (let new-d nil
                  (prn desc)
                  (= new-d (f old-d))
                  (when debugmode
                    (w/appendfile stream "arc2c.log"
                      (w/stdout stream
                        (prn "--------------------------")
                        (prn desc)
                        (prn "--------------------------")
                        (ppr-sexp (source new-d)))))
                  new-d))
              chain d))
    (prn "Writing C code...")
    (w/outfile f (+ (strip-ext filename) ".c")
      (w/stdout f
        (prn:liststr:code-generate d)))
    t))
