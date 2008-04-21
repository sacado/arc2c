obj global[NB_GLOBALS];
obj stack[MAX_STACK];
freelist freel;
obj * closure;
obj * sp;

obj SYM2OBJ (char * s){ /* Find a symbol, or save it if it's the first time */
  static symbol ** syms = NULL;
  static int max_syms   = 0;
  static int nsyms      = 0;
  int i;

  for (i = 0 ; i < nsyms ; i++)
    if (strcmp (s, syms[i]->value) == 0) /* found it */
      return (obj) syms[i];

  /* Not found, add the symbol after possible realloc */

  if (nsyms == max_syms){ /* Needs more space */
    max_syms = (max_syms == 0 ? INITIAL_MAX_SYMS : max_syms * 2);
    syms = realloc (syms, sizeof(symbol *) * max_syms);

    if (syms == NULL){
      fprintf (stderr, "Out of memory when allocating new symbol, sorry...\n");
      exit(1);
    }
  }

  syms[nsyms]        = malloc (sizeof(symbol)); /* Symbols never go away, then can be malloc-ed */
  syms[nsyms]->type  = T_SYM;
  syms[nsyms]->value = (char *) malloc (strlen (s) + 1);
  strcpy (syms[nsyms]->value, s);

  return (obj) syms[nsyms++];
}


char eq_str (string * a, string * b){
  int i;

  if (a->size != b->size)
    return 0;

  for (i = 0 ; i < a->size ; i++)
    if (a->cpts[i] != b->cpts[i])
      return 0;

  return 1;
}


char * cpt2utf8 (long cpt){
  char * res = (char *) malloc (4 + 1);
  int len    = 0;

  if (cpt < 128)
    res[len++] = (char) cpt;

  else if (cpt < 2048){ /* 2 bytes needed*/
    res[len++] = (cpt / 64) | 192;
    res[len++] = (cpt % 64) | 128;
  }

  else if (cpt < 65536){ /* 3 bytes needed*/
    res[len++] = (cpt / 4096) | 224;
    cpt        = cpt % 4096;
    res[len++] = (cpt / 64) | 128;
    res[len++] = (cpt % 64) | 128;
  }

  else{ /* 4 bytes needed*/
    res[len++] = (cpt / 262144) | 240;
    cpt        = cpt % 262144;
    res[len++] = (cpt / 4096 ) | 128;
    cpt        = cpt % 4096;
    res[len++] = (cpt / 64) | 128;
    res[len++] = (cpt % 64) | 128;
  }

  res[len] = '\0';

  return res;
}


char * str2utf8 (string * s){
  /* Just get as much memory as we *could* need */
  char * res = (char *) malloc ((s->size * 4) + 1);
  int i;
  int len = 0;
  long cur;

  for (i = 0 ; i < s->size ; i++){
    cur = s->cpts[i];

    if (cur < 128)
      res[len++] = (char) cur;

    else if (cur < 2048){ /* 2 bytes needed*/
      res[len++] = (cur / 64) | 192;
      res[len++] = (cur % 64) | 128;
    }

    else if (cur < 65536){ /* 3 bytes needed*/
      res[len++] = (cur / 4096) | 224;
      cur        = cur % 4096;
      res[len++] = (cur / 64) | 128;
      res[len++] = (cur % 64) | 128;
    }

    else{ /* 4 bytes needed*/
      res[len++] = (cur / 262144) | 240;
      cur        = cur % 262144;
      res[len++] = (cur / 4096 ) | 128;
      cur        = cur % 4096;
      res[len++] = (cur / 64) | 128;
      res[len++] = (cur % 64) | 128;
    }
  }

  res[len+1] = '\0';

  return realloc(res, len+1);
}


string * utf82str (char * s){
  int size     = strlen(s);
  string * res = (string *) gc_malloc(sizeof(string));
  int i = 0;
  int len = 0;
  unsigned char cur;
  long cpt;

  res->cpts = malloc (sizeof(long) * size); /* Can be too big */
  res->type = T_STR;

  while (i < size){
    cur = (unsigned char) s[i];
    if (cur < 128) /* An ASCII char */
      res->cpts[len++] = (long) s[i++];
    else if ((cur & 240) == 240){ /* 4 bytes */
      cpt = (((unsigned char) s[i++]) & ~240) * 262144;
      cpt += (((unsigned char) s[i++]) & ~128) * 4096;
      cpt += (((unsigned char) s[i++]) & ~128) * 64;
      cpt += (((unsigned char) s[i++]) & ~128);
    }
    else if ((cur & 224) == 224){ /* 3 bytes */
      cpt = (((unsigned char) s[i++]) & ~224) * 4096;
      cpt += (((unsigned char) s[i++]) & ~128) * 64;
      cpt += (((unsigned char) s[i++]) & ~128);
      res->cpts[len++] = cpt;
    }
    else{ /* 2 bytes */
      cpt = (((unsigned char) s[i++]) & ~192) * 64;
      cpt += (((unsigned char) s[i++]) & ~128);
      res->cpts[len++] = cpt;
    }
  }

  res->size = len;

  return res;
}


string * string_concat (string * s1, string * s2){
  string * res = (string *) gc_malloc(sizeof(string));

  res->type = T_STR;
  res->size = s1->size + s2->size;
  res->cpts = malloc(sizeof(long) * res->size);

  memcpy (res->cpts, s1->cpts, sizeof(long) * s1->size);
  memcpy (res->cpts + s1->size, s2->cpts, sizeof(long) * s2->size);

  return res;
}


void set_tbl (table * t, obj index, obj value){
  int i;
  obj cur_key;

  for (i = 0 ; i < t->size ; i++){
    cur_key = t->keys[i];

    if (!APTR(cur_key) || !ASTR(cur_key)){
      if (cur_key == index)
        break;
    }
    else if (ASTR(cur_key))
      if (eq_str ((string *) cur_key, (string *) index))
        break;
  }

  if (i == t->max_size){
    t->max_size *= 2;
    t->keys     = realloc (t->keys, sizeof (obj) * t->max_size);
    t->values   = realloc (t->values, sizeof (obj) * t->max_size);
  }

  if (i == t->size)
    t->size++;

  t->keys[i] = index;
  t->values[i] = value;
}


obj tbl_lookup (table * t, obj index){
  int i;
  obj cur_key;

  for (i = 0 ; i < t->size ; i++){
    cur_key = t->keys[i];

    if (!APTR(cur_key) || !ASTR(cur_key)){
      if (cur_key == index)
        return t->values[i];
    }
    else if (ASTR(cur_key))
      if (eq_str ((string *) cur_key, (string *) index))
        return t->values[i];
  }

  return SYM2OBJ("nil");
}


obj cons_fun(obj a, obj d){
  pair * p = (pair *) gc_malloc(sizeof(pair));
  p->type = T_PAIR;
  p->car = a;
  p->cdr = d;
  return (obj) p;
}


void pr_tbl (table * t){
  int i;
  obj dummy;

  printf ("#hash(");

  for (i = 0 ; i < t->size ; i++){
    printf("(");
    PUSH(t->keys[i]);
    PR();
    dummy = POP();

    printf(" . ");
    PUSH(t->values[i]);
    PR();
    dummy = POP();
    printf(")");

    if (i < t->size - 1)
      printf (" ");
  }

  printf (")");
}


void PR(){
  pair * p;
  tagged * t;
  obj y = TOS();

  if (AFIX(y))
    printf ("%ld", OBJ2FIX(y));
  else if (ACHAR(y)){
    char * utf8 = cpt2utf8(OBJ2CHAR(y));
    printf ("%s", utf8);
    free(utf8);
  }
  else if (ASYM(y))
    printf ("%s", ((symbol *)y)->value);
  else if (ASTR(y))
    printf ("%s", str2utf8 ((string *) y));
  else if (AFN(y))
    printf ("#<procedure>");
  else if (ATBL(y))
    pr_tbl((table *) y);
  else if (ATAG(y)){
    t = (tagged *) y;
    printf ("#3(tagged ");
    PUSH(t->ctype); PR();
    printf (" ");
    TOS() = t->content; PR(); sp--; 
    printf (")");
  }
  else if (APAIR(y)){
    printf ("(");

    while (APAIR(y)){
        p = (pair *) y;
        PUSH(p->car); PR(); sp--;
        y = p->cdr;

        if (APAIR(y))
          printf (" ");
   }

   if (y != SYM2OBJ("nil")){
       PUSH(y);
       printf (" . ");
       PR(); sp--;
    }
    printf (")");
  }
}


void gc_init(){
  int i;

  freel.nbfree = HEAP_SIZE;

  for (i = 0 ; i < HEAP_SIZE ; i++){
    freel.free[i] = i;
    freel.mark[i] = FREE;
  }
}


void explore_heap(obj from){
  long o;
  long * fn;
  table * t;
  int i;
  obj * pfrom;

  if (APTR(from)){
    pfrom = (void *) from;
    i     = 0;

    while (i < HEAP_SIZE && (freel.heap[i] != from || freel.mark[i] == FREE))
      i++;

    if (i < HEAP_SIZE && freel.mark[i] == UNMARKED){
      freel.mark[i] = MARKED;
      o = (long) *pfrom;

      switch (o){
        case T_PAIR:
          explore_heap(((pair *)pfrom)->car);
          explore_heap(((pair *)pfrom)->cdr);
          break;
        case T_TAG:
          explore_heap(((tagged *)pfrom)->ctype);
          explore_heap(((tagged *)pfrom)->content);
          break;
        case T_STR:
          break;
        case T_FN:
          fn = pfrom;

          for (i = 0 ; i < fn[1]; i++)
            explore_heap((obj)(fn[i+3]));
          break;
        case T_TBL:
          t = (table *) pfrom;

          for (i = 0 ; i < t->size ; i++){
            explore_heap(t->keys[i]);
            explore_heap(t->values[i]);
          }

          break;
      }
    }
  }
}


void perform_gc(){
  int i;
  obj * scur;
  string * s;

  for (scur = stack ; scur < sp ; scur++)
    explore_heap (*scur);

  for (i = 0; i < NB_GLOBALS ; i++)
    explore_heap (GLOBAL(i));

  for (i = 0 ; i < NB_QUOTE_CONSTANTS ; i++)
    explore_heap (QUOTE_CONSTANTS[i]);

  for (i = 0 ; i < HEAP_SIZE ; i++){
    switch (freel.mark[i]){
      case UNMARKED:
        if (ATBL(freel.heap[i])){
          t = (table *) (freel.heap[i]);
          free(t->keys);
          free(t->values);
        }
        else if (ASTR(freel.heap[i])){
          s = (string *) (freel.heap[i]);
          free (s->cpts);
        }

        free((void *)(freel.heap[i]));
        freel.mark[i] = FREE;
        freel.free[freel.nbfree++] = i;
        break;
      case MARKED:
        freel.mark[i] = UNMARKED;
        break;
    }
  }
}


obj gc_malloc (size_t size){
  int i = 0;
  obj res;

  if (freel.nbfree == 0)
    perform_gc();

  i = freel.free[--freel.nbfree];

  if (freel.mark[i] != FREE)
    printf ("%d\n", freel.mark[i]);

  res = (obj) malloc(size);
  freel.heap[i] = res;
  freel.mark[i] = UNMARKED;

  return res;
}


int main (int argc, char * argv[]) {
  GC_INIT();
  gc_init();
  init_constants();
  execute(0);
  return 0;
}

