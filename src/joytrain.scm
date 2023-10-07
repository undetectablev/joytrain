;; JoyTrain, a pseudorandom algebraic multiquine relay

(define *matching-probability-1/n* 20)
(define *number-of-passes* 5)

(define *axioms*
  '(
    (  dup == dup swap                       )
    (  cons == swap swons                    )
    (  swons == swap cons                    )
    (  uncons == unswons swap                )
    (  uncons == dup first swap rest         )
    (  uncons == dup [first] dip rest        )
    (  unswons == uncons swap                )
    (  unswons == dup rest swap first        )
    (  unswons == dup [rest] dip first       )
    (  first == uncons pop                   )
    (  rest == unswons pop                   )
    (  first == dup first [pop] dip          )
    (  rest == dup rest [pop] dip            )
    (  cons == [[] cons] dip concat          )
    (  [concat] dip concat == concat concat  )
    
    (  dip pop == [pop] dip i                )
    (  i == dup dip pop                      )
    (  i == [] cons i i                      )
    (  i == [i] cons i                       )
    
    (  [?f *p] [?f *a] [?f *b] ifte -> ?f [*p] [*a] [*b] ifte  )
    (  [*p] [+a -f] [+b -f] ifte -> [*p] [+a] [+b] ifte -f     )
    (  ?f [*p] [*a] [*b] ifte -> [?f *p] [?f *a] [?f *b] ifte  )
    (  [*p] [*a] [*b] ifte ?f -> [*p] [*a ?f] [*b ?f] ifte     )
    ))

(define *ad-hoc-axioms*
  '(
    (  [id] ifte -> [[] i] ifte              )
    (  [id] ifte -> [[] dip] ifte            )
    (  [id] ifte -> [[] concat] ifte         )
    (  [id] ifte -> [[] swap concat] ifte    )
    (  [id] ifte -> [[] cons first] ifte     )
    (  [id] ifte -> [swap swap] ifte         )
    (  [id] ifte -> [dup pop] ifte           )
    (  [id] ifte -> [cons uncons] ifte       )
    (  [id] ifte -> [uncons cons] ifte       )
    (  [id] ifte -> [swons unswons] ifte     )
    (  [id] ifte -> [unswons swons] ifte     )
    ))

(define *normalization-rules*
  '(
    (  ?c id -> ?c                           )
    (  id ?c -> ?c                           )
    (  id <- [] i                            )
    (  id <- [] dip                          )
    (  id <- [] concat                       )
    (  id <- [] swap concat                  )
    (  id <- [] cons first                   )
    (  id <- swap swap                       )
    (  id <- [] pop                          )
    (  id <- dup pop                         )
    (  id <- cons uncons                     )
    (  id <- uncons cons                     )
    (  id <- swons unswons                   )
    (  id <- unswons swons                   )
    
    (  pop <- swap [pop] dip                 )
    (  dup <- dup swap                       )
    (  cons <- swap swons                    )
    (  swons <- swap cons                    )
    (  unswons <- uncons swap                )
    (  uncons <- unswons swap                )
    (  uncons <- dup first swap rest         )
    (  uncons <- dup [first] dip rest        )
    (  unswons <- dup rest swap first        )
    (  unswons <- dup [rest] dip first       )
    (  first <- uncons pop                   )
    (  rest <- unswons pop                   )
    (  first <- dup first [pop] dip          )
    (  rest <- dup rest [pop] dip            )
    (  cons <- [[] cons] dip concat          )

    (  i <- [] cons i i                      )
    (  i <- [i] cons i                       )
    (  i <- dup dip pop                      )
    (  dip pop <- [pop] dip i                )
    
    (  [?f *p] [?f *a] [?f *b] ifte -> ?f [*p] [*a] [*b] ifte  )
    (  [*p] [+a -f] [+b -f] ifte -> [*p] [+a] [+b] ifte -f     )
    ))

(define emit-dign
  (lambda (n)
    `([] ,@(map (lambda (x) 'cons) (iota n)) dip)))

(define emit-buryn
  (lambda (n)
    `([[] ,@(map (lambda (x) 'cons) (iota n))] dip swap i)))

(define emit-times
  (lambda (n)
    (fold-right append '() (map (lambda (x) '[dup dip]) (iota n)))))

(define emit-ycomb
  (lambda (p)
    `(,@p [dup cons] swap concat dup cons i)))

(define emit-linrec
  (lambda (i t e1 e2)
    (emit-ycomb `([[pop ,@i] [pop ,@t] [,e1 dip i ,@e2] ifte]))))

(define emit-binrec
  (lambda (i t e1 e2)
    (emit-ycomb `([[pop ,@i] [pop ,@t] [,e1 dip dup [dip] dip i ,@e2] ifte]))))

(define emit-quine-relay
  (lambda (p)
    `(,p [[dup i] dip dup cons cons] [dup i] dip dup cons cons)))

(define shrink
  (lambda (ls)
    (reverse (cdr (reverse ls)))))

(define var?
  (lambda (x)
    (let ((v (string-ref (symbol->string x) 0)))
      (or (equal? v #\?)
          (equal? v #\*)
          (equal? v #\+)
          (equal? v #\-)))))

(define var*?
  (lambda (x)
    (equal? (string-ref (symbol->string x) 0) #\*)))

(define var+?
  (lambda (x)
    (equal? (string-ref (symbol->string x) 0) #\+)))

(define var-?
  (lambda (x)
    (equal? (string-ref (symbol->string x) 0) #\-)))

(define split-axiom
  (lambda (a r)
    (cond ((equal? (car r) '->) `((,a ,(cdr r))))
          ((equal? (car r) '<-) `((,(cdr r) ,a)))
          ((equal? (car r) '==) `((,a ,(cdr r)) (,(cdr r) ,a)))
          (else (split-axiom (append a `(,(car r))) (cdr r))))))

(define emit-var-checks
  (lambda (p)
    (let ((b (filter pair? (emit-binds p '())))
          (v '()))
      (map
       (lambda (a)
         (set! v (if (assoc (car a) v)
                     (map
                      (lambda (x)
                        (if (equal? (car x) (car a))
                            (cons (car x) (+ 1 (cdr x)))
                            x)) v)
                     (cons (cons (car a) 1) v))))
       b)
      (map
       (lambda (x)
         (let ((vb (filter (lambda (y) (equal? (car y) (car x))) b)))
           (cond ((= 2 (cdr x)) `(dup
                                  first ,@(cdar vb) swap
                                  first ,@(cdadr vb) =))
                 ((= 3 (cdr x)) `(dup
                                  first ,@(cdar vb) swap dup
                                  first ,@(cdadr vb) swap
                                  first ,@(cdaddr vb)
                                  dup [= swap] dip = [swap] [swap pop] [pop] ifte ;and
                                  )))))
       (filter (lambda (x) (> (cdr x) 1)) v)))))

(define emit-checks
  (lambda (p a)
    (cond ((null? p) `((,@a null)))
          ((pair? p) (append
                      `((,@a pair))
                      (emit-checks (car p) `(,@a first))
                      (emit-checks (cdr p) `(,@a rest))))
          ((var? p) '())
          (else `((,@a ,(symbol->string p) intern =))))))

(define emit-binds
  (lambda (p a)
    (cond ((null? p) '(()))
          ((pair? p) (append (emit-binds (car p) `(,@a first))
                             (emit-binds (cdr p) `(,@a rest))))
          ((var*? p) `((,p ,@(shrink a))))
          ((var+? p) `((,p ,@(shrink a)
                           [[]] dip
                           ,@(emit-linrec '[rest null]
                                          '[]
                                          '[uncons [[] cons] dip]
                                          '[[concat] dip]) pop)))
          ((var-? p) `((,p ,@(shrink a)
                           [[]] dip
                           ,@(emit-linrec '[rest null]
                                          '[]
                                          '[uncons [[] cons] dip]
                                          '[[concat] dip]) swap pop)))
          ((var? p)  `((,p ,@a)))
          (else      '()))))

(define emit-subst
  (lambda (p b n)
    (if (pair? p)
        `(,@(cond ((null? (car p)) `([] [] cons concat))
                  ((pair? (car p)) `([] ,@(emit-subst (car p) b (+ 1 n)) [] cons concat))
                  ((var*? (car p)) `(,@(emit-dign n)
                                     dup ,@(cdr (assoc (car p)
                                                       (filter (lambda (x) (pair? x)) b)))
                                     [,@(emit-buryn n)] dip concat))
                  ((var+? (car p)) `(,@(emit-dign n)
                                     dup ,@(cdr (assoc (car p)
                                                       (filter (lambda (x) (pair? x)) b)))
                                     [[]] dip
                                     ,@(emit-linrec '[rest null]
                                                    '[]
                                                    '[uncons [[] cons] dip]
                                                    '[[concat] dip]) concat
                                                    [,@(emit-buryn n)] dip concat))
                  ((var-? (car p)) `(,@(emit-dign n)
                                     dup ,@(cdr (assoc (car p)
                                                       (filter (lambda (x) (pair? x)) b)))
                                     [[]] dip
                                     ,@(emit-linrec '[rest null]
                                                    '[]
                                                    '[uncons [[] cons] dip]
                                                    '[[concat] dip]) swap pop
                                                    [,@(emit-buryn n)] dip concat))
                  ((var?  (car p)) `(,@(emit-dign n)
                                     dup ,@(cdr (assoc (car p)
                                                       (filter (lambda (x) (pair? x)) b)))
                                     [] cons
                                     [,@(emit-buryn n)] dip concat))
                  (else   `(,(symbol->string (car p)) intern [] cons concat)))
          ,@(emit-subst (cdr p) b n))
        '())))

(define filter-checks
  (lambda (checks)
    (fold-right
     (lambda (c1 c2)
       (if (and (pair? c1)
                (pair? c2)
                (> (length (car c2)) 1)
                (not (equal? '= (car (reverse c1))))
                (equal? 'null (car (reverse (car c2))))
                (equal? 'rest (cadr (reverse (car c2)))))
           (cons c1 (cdr c2))
           (cons c1 c2)))
     '()
     checks)))

(define wrap-in-ifte
  (lambda (checks code)
    (fold-right
     (lambda (a b)
       `(,a ,b [id] ifte))
     code
     checks
     )))

(define emit-matcher
  (lambda (axioms rand-init rand-check rand-step)
    `(,@rand-init
      ,@(emit-binrec
         '[first atom]
         '[]
         `[,@(fold-right
              append
              '()
              (map
               (lambda (a)
                 `(,@(wrap-in-ifte
                      (append (filter-checks
                               (append
                                rand-check
                                (shrink (cdr (emit-checks (car a) '(first))))))
                              (emit-var-checks (car a)))
                      `(unswons [] ,@(emit-subst (cadr a) (emit-binds (car a) '()) 1)
                                ,(map (lambda (i) 'rest) (car a))
                                dip swap concat swap cons
                                ))
                   ,@rand-step))
               (fold-right
                append
                '()
                (map
                 (lambda (a)
                   (split-axiom '() a))
                 axioms))))
           uncons [uncons] dip dup [swap] dip cons [cons] dip
           ]
         '[first [unswons] dip cons swons])
      first)))

(define emit-body
  (lambda ()
    `(
      dup
      ,(emit-matcher (append *axioms* *ad-hoc-axioms*)
                     `[[[rest rest rest first first
                         ,*matching-probability-1/n* div 1 =]
                        [uncons uncons uncons first unswons
                         [0 =] [pop dup concat]
                         [,*matching-probability-1/n* div pop swons] ifte
                        [] cons cons cons cons]]
                       cons rand dup [] cons cons [] cons concat]
                     '([dup rest first i])
                     '[dup rest rest first i])
      ,@(emit-times *number-of-passes*)
      swap [] swap
      ,@(emit-linrec '[=]
                     '[pop swap i]
                     `[swap pop dup
                       ,@(emit-matcher *normalization-rules*
                                       '([] [] cons cons) '() '())]
                     '[])
      [pop] dip
      )))

(define joy-brackets
  (lambda (s)
    (list->string
     (map
      (lambda (c)
        (case c
          (#\( #\[)
          (#\) #\])
          (else c)))
      (string->list s)))))

(call-with-string-output-port
 (lambda (p)
   (pretty-print (emit-quine-relay (emit-body)) p)
   (display (joy-brackets (get-output-string p)))))
