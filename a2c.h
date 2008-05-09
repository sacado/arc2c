#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HEAP_SIZE 500000
#define FREE 0
#define MARKED 1
#define UNMARKED 2

#define INITIAL_MAX_SYMS 50
#define T_PAIR      0
#define T_SYM       1
#define T_TAG       2
#define T_STR       3
#define T_FN        4
#define T_TBL       5
#define T_SHAREDVAR 6
#define T_FLOAT     7

typedef long obj;

typedef struct {
  long type; /* T_PAIR */
  obj car;
  obj cdr;
} pair;

typedef struct {
  long type; /* T_SYM */
  char * value; /* UTF-8 chars */
} symbol;

typedef struct {
  long type; /* T_TAG */
  obj ctype;
  obj content;
} tagged;

typedef struct {
  long type; /* T_STR */
  int size;
  long * cpts; /* codepoints */
} string;

typedef struct {
  long type; /* T_TBL */
  int size;
  int max_size;
  obj * keys;
  obj * values;
} table;

typedef struct {
  long type; /* T_FLOAT */
  double value;
} flonum;

typedef struct {
  long type; /*T_SHAREDVAR*/
  obj var;
} sharedvar;

typedef struct {
  int nbfree;
  int free[HEAP_SIZE]; /*indexes of free elements*/
  obj heap[HEAP_SIZE]; /* ptr to ith object */
  char mark[HEAP_SIZE]; /* mark of ith object */
} freelist;

#define AFIX(o) (((o) & 3) == 3)     /* the last bits are 11 : it's a fixnum */
#define ACHAR(o) (((o) & 3) == 1) /* the last bits are 01 : it's a char */
#define APTR(o) (!((o) & 1))    /* the last bit is 0 : it's a ref to something else */
#define ASYM(o) (((obj*)(o))[0] == T_SYM)
#define APAIR(o) (((obj*)(o))[0] == T_PAIR)
#define ATAG(o) (((obj*)(o))[0] == T_TAG)
#define ASTR(o) (((obj*)(o))[0] == T_STR)
#define AFN(o) (((obj*)(o))[0] == T_FN)
#define ATBL(o) (((obj*)(o))[0] == T_TBL)
#define AFLOAT(o) (((obj*)(o))[0] == T_FLOAT)

#define FIX2OBJ(n) (((n) << 2) + 3)
#define OBJ2FIX(o) ((o) >> 2)
#define CHAR2OBJ(c) (((c) << 2) + 1)
#define OBJ2CHAR(o) ((o) >> 2)

obj DBL2OBJ (double d);
obj SYM2OBJ (char * s); /* Find a symbol, or save it if it's the first time */

#define GLOBAL(i) global[i]
#define LOCAL(i) stack[i]
#define CLOSURE_REF(self,i) ((obj *)(self))[i+2]

#define TOS() sp[-1]
#define PUSH(x) *sp++ = x
#define POP() *--sp

char eq_str (string * a, string * b);

#define EQ() { obj y = POP();\
  if (APTR(y) && ASTR(y))\
    TOS() = eq_str ((string *) y, (string *) TOS()) ? TOBJ : NILOBJ;\
  else if (APTR(y) && AFLOAT(y))\
    TOS() = ((flonum *)y)->value == ((flonum *)TOS())->value ? TOBJ : NILOBJ;\
  else\
    TOS() = TOS() == y ? TOBJ : NILOBJ; }

#define NEQ() { EQ(); TOS() = (TOS() == TOBJ) ? NILOBJ : TOBJ; }

#define TYPE() { obj * p; obj y = TOS();\
  if (APTR(y) && ATAG(y)){\
    p     = (obj *) y;\
    TOS() = ((tagged *)p)->ctype;\
  }\
  else if (AFIX(y))\
    TOS() = SYM2OBJ("int");\
  else if (ACHAR(y))\
    TOS() = SYM2OBJ("char");\
  else{\
    p = (obj *) y;\
    switch ((char) *p){\
      case T_PAIR: TOS() = SYM2OBJ("cons"); break;\
      case T_SYM: TOS() = SYM2OBJ("sym"); break;\
      case T_STR: TOS() = SYM2OBJ("string"); break;\
      case T_FN: TOS() = SYM2OBJ("fn"); break;\
      case T_TBL: TOS() = SYM2OBJ("table"); break;\
      case T_FLOAT: TOS() = SYM2OBJ("num"); break;\
    }\
  }\
}

char * str2utf8 (string * s);
string * utf82str (char * s);
string * string_concat (string * s1, string * s2);

void set_tbl (table * t, obj index, obj value);
obj tbl_lookup (table * t, obj index);

#define TBL() { table * t = (table *) gc_malloc(sizeof(table));\
t->type = T_TBL;\
t->size = 0;\
t->max_size = 10;\
t->keys = (obj *) malloc(sizeof(obj) * 10);\
t->values = (obj *) malloc(sizeof(obj) * 10);\
PUSH((obj)t);\
}

#define ANNOTATE() { tagged * t = (tagged *) gc_malloc(sizeof(tagged)); t->type = T_TAG; t->content = POP(); t->ctype = POP(); PUSH((obj)t); }
#define REP() { obj y = TOS(); if (APTR(y) && ATAG(y)) TOS() = ((tagged*)y)->content; }

#define LT() { obj y = POP();\
  if (AFIX(y) || ACHAR(y))\
    TOS() = TOS() < y ? TOBJ : NILOBJ;\
  else if (AFLOAT(y))\
    TOS() = ((flonum*)TOS())->value < ((flonum*)y)->value ? TOBJ : NILOBJ;\
}

#define GT() { obj y = POP();\
  if (AFIX(y) || ACHAR(y))\
    TOS() = TOS() > y ? TOBJ : NILOBJ;\
  else if (AFLOAT(y))\
    TOS() = ((flonum*)TOS())->value > ((flonum*)y)->value ? TOBJ : NILOBJ;\
}

#define LE() { obj y = POP();\
  if (AFIX(y) || ACHAR(y))\
    TOS() = TOS() <= y ? TOBJ : NILOBJ;\
  else if (AFLOAT(y))\
    TOS() = ((flonum*)TOS())->value <= ((flonum*)y)->value ? TOBJ : NILOBJ;\
}

#define GE() { obj y = POP();\
  if (AFIX(y) || ACHAR(y))\
    TOS() = TOS() >= y ? TOBJ : NILOBJ;\
  else if (AFLOAT(y))\
    TOS() = ((flonum*)TOS())->value >= ((flonum*)y)->value ? TOBJ : NILOBJ;\
}

#define ADD() { obj y = POP(); obj z = POP();\
  if (AFIX(y) && AFIX(z))\
    PUSH(FIX2OBJ(OBJ2FIX(z) + OBJ2FIX(y)));\
  else if (APTR(y) && AFLOAT(y) || APTR(z) && AFLOAT(z)){\
    double a, b;\
    flonum * res = (flonum *) gc_malloc (sizeof(flonum));\
    res->type = T_FLOAT;\
    a = (AFIX(y) ? (double) OBJ2FIX(y) : ((flonum*)y)->value);\
    b = (AFIX(z) ? (double) OBJ2FIX(z) : ((flonum*)z)->value);\
    res->value = a + b;\
    PUSH((obj)res);\
  }\
  else if (APTR(y) && ASTR(y))\
    TOS() = (obj) string_concat ((string*) y, (string*) TOS());\
}
#define SUB() { obj y = POP(); obj z = POP();\
  if (AFIX(y) && AFIX(z))\
    PUSH(FIX2OBJ(OBJ2FIX(z) - OBJ2FIX(y)));\
  else{\
    /* One of them is a flonum */\
    double a, b;\
    flonum * res = (flonum *) gc_malloc(sizeof(flonum));\
    res->type = T_FLOAT;\
    a = (AFIX(y) ? (double) OBJ2FIX(y) : ((flonum*)y)->value);\
    b = (AFIX(z) ? (double) OBJ2FIX(z) : ((flonum*)z)->value);\
    res->value = b - a;\
    PUSH((obj)res);\
  }}
#define MUL() { obj y = POP(); obj z = POP();\
  if (AFIX(y) && AFIX(z))\
    PUSH(FIX2OBJ(OBJ2FIX(z) * OBJ2FIX(y)));\
  else{\
    /* One of them is a flonum */\
    double a, b;\
    flonum * res = (flonum *) gc_malloc(sizeof(flonum));\
    res->type = T_FLOAT;\
    a = (AFIX(y) ? (double) OBJ2FIX(y) : ((flonum*)y)->value);\
    b = (AFIX(z) ? (double) OBJ2FIX(z) : ((flonum*)z)->value);\
    res->value = b * a;\
    PUSH((obj)res);\
  }}

#define LEN() { obj y = TOS(); long r;\
  if (ASTR(y))\
    TOS() = FIX2OBJ(((string *) y)->size);\
  else{\
    r = 0;\
    while (y != SYM2OBJ("nil")){\
	    y = ((pair *) y)->cdr;\
   	 r++;\
    }\
    TOS() = FIX2OBJ(r);\
  }\
}

obj cons_fun(obj a, obj d);

#define CONS() { pair * p = (pair *) gc_malloc(sizeof(pair)); p->type = T_PAIR ; p->cdr = POP(); p->car = POP(); PUSH((obj)p); }
#define CAR() { if (TOS() != NILOBJ) {pair * p = (pair *) POP(); PUSH((obj)(p->car)); }}
#define CDR() { if (TOS() != NILOBJ) {pair * p = (pair *) POP(); PUSH((obj)(p->cdr)); }}

#define SREF() { obj idx, val, var; string * s; table * t;\
  idx = POP(); val = POP(); var = TOS();\
  if (ASTR(var)){\
    s = (string *) var;\
    s->cpts[OBJ2FIX(idx)] = OBJ2CHAR(val);\
  }\
  else if (ATBL(var)){\
    t = (table *) var;\
    set_tbl (t, idx, val);\
  }\
}

#define MAKE_SHAREDVAR() {sharedvar * p = (sharedvar *) gc_malloc(sizeof(sharedvar)); p->type = T_SHAREDVAR; p->var = POP(); PUSH((obj)p);}
#define READ_SHAREDVAR() {sharedvar * p = (sharedvar *) POP(); PUSH((obj)(p->var));}
#define WRITE_SHAREDVAR() {obj v = POP(); sharedvar * p = (sharedvar *) POP(); p->var = v; PUSH(v);}

#define PRN() { PR(); printf ("\n");}
void PR();

#define HALT() break

//place arguments exceeding nbreq into a list at the top of
//the stack
#define VARIADIC2LIST(nbreq) for(PUSH(NILOBJ); num_args > nbreq; num_args--) CONS()

#define BEGIN_CLOSURE(label,nbfree) closure = (obj *) gc_malloc(sizeof(obj) * (nbfree + 3));
#define INICLO(i) closure[i+2] = POP();
#define END_CLOSURE(label,nbfree) closure[0] = T_FN; closure[1] = nbfree; closure[2] = label; PUSH((obj)closure);

#define BEGIN_JUMP(nbargs) {sp = stack; num_args = nbargs;}
#define END_JUMP(nbargs) { obj o = LOCAL(0);\
if (!AFN(o)){\
  BEGIN_JUMP(2); PUSH(LOCAL(1));\
  if (ATBL(o)){\
    PUSH(tbl_lookup((table *) o, LOCAL(2)));\
  } else if (ASTR(o)){\
    PUSH(CHAR2OBJ(((string *) o)->cpts[OBJ2FIX(LOCAL(2))]));\
  } else if (APAIR(o)){\
    long idx = OBJ2FIX(LOCAL(2));\
    while (idx > 0){ idx--; o = ((pair *) o)->cdr; }\
    PUSH(((pair *) o)->car);\
  }\
}\
closure = (obj *) LOCAL(0); pc = closure[2]; goto jump;}


void gc_init();
void explore_heap(obj from);
void perform_gc();
obj gc_malloc (size_t size);

obj execute (int pc);

obj QUOTE_CONSTANTS[NB_QUOTE_CONSTANTS];

obj global[NB_GLOBALS];
obj stack[MAX_STACK];
freelist freel;
obj * closure;
obj * sp;

