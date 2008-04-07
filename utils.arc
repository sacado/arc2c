; utilities

(def diff (s1 s2)
  (if
    (no s1)
      '()
    (some (car s1) s2)
      (diff (cdr s1) s2)
      (cons (car s1) (diff (cdr s1) s2))))

(def union (s1 s2)
  (if
    (no s1)
      s2
    (some (car s1) s2)
      (union (cdr s1) s2)
      (cons (car s1) (union (cdr s1) s2))))

(def union-multi (sets)
  (foldl union '() sets))

(def foldl (f base lst)
  (if (no lst)
    base
    (foldl f (f base (car lst)) (cdr lst))))

(def equalset (s1 s2)
  " Determines if two sets are equal.  Works correctly only
    if both sets do not have duplicate elements. "
  (and (is (len s1) (len s2))
       ; assuming s1 doesn't have duplicates, should work properly
       (all [some _ s2] s1)))

(def liststr (lst)
  (let result ""
    (each elt lst
      (if (alist elt)
        (++ result (liststr elt))
        (++ result (string elt))))
    result))

(def properify (lst)
  " Makes an improper list proper, or a non-list into a
    singleton list "
  (if (alist lst) (makeproper lst) (cons lst nil)))

(def map-improper (f l)
  " A mapping function which supports both
    proper and improper lists; mapping on an
    improper list returns an improper list "
  (if
    (acons l)
      (cons (f:car l) (map-improper f (cdr l)))
    l
      (f l)))

;------------------------------------------------------------------------------

; free variables

(def fv (ast)
  (if
    (aref ast)
      (list ast!var)
    (aset ast)
      (union (fv (car ast!subx)) (list ast!var))
    (alam ast)
      (diff (fv (car ast!subx)) (properify ast!params))
    (aquote ast)
      nil
      (union-multi (map fv ast!subx))))

; local variables that are set

(def lv-set (ast)
  (if
    (and (aset ast) (~aglobal ast!var))
      (union (lv-set (car ast!subx)) (list ast!var))
    (aquote ast)
      nil
      (union-multi (map lv-set ast!subx))))

; global variables that are set

(def gv-set (ast)
  (if
    (and (aset ast) (aglobal ast!var))
      (union (gv-set (car ast!subx)) (list ast!var))
    (aquote ast)
      nil
      (union-multi (map gv-set ast!subx))))

; global variables that are read

(def gv-read (ast)
  (if
    (and (aref ast) (aglobal ast!var))
      (list ast!var)
    (aquote ast)
      nil
      (union-multi (map gv-read ast!subx))))

