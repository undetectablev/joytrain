;; This is a simple interpreter for a small subset of Joy.

(define joy-eval
  (lambda (p s)
    (cond 
     ((pair? p)
      (case (car p)
        ('id      (joy-eval (cdr p) s))
        ('i       (joy-eval (cdr p) (joy-eval (car s) (cdr s))))
        ('dip     (joy-eval (cdr p) (cons (cadr s) (joy-eval (car s) (cddr s)))))
        ('pop     (joy-eval (cdr p) (cdr s)))
        ('dup     (joy-eval (cdr p) (cons (car s) s)))
        ('swap    (joy-eval (cdr p) (cons (cadr s) (cons (car s) (cddr s)))))
        ('cons    (joy-eval (cdr p) (cons (cons (cadr s) (car s)) (cddr s))))
        ('swons   (joy-eval (cdr p) (cons (cons (car s) (cadr s)) (cddr s))))
        ('uncons  (joy-eval (cdr p) (cons (cdar s) (cons (caar s) (cdr s)))))
        ('unswons (joy-eval (cdr p) (cons (caar s) (cons (cdar s) (cdr s)))))
        ('concat  (joy-eval (cdr p) (cons (append (cadr s) (car s)) (cddr s))))
        ('first   (joy-eval (cdr p) (cons (car (car s)) (cdr s))))
        ('rest    (joy-eval (cdr p) (cons (cdr (car s)) (cdr s))))
        ('+       (joy-eval (cdr p) (cons (+ (cadr s) (car s)) (cddr s))))
        ('-       (joy-eval (cdr p) (cons (- (cadr s) (car s)) (cddr s))))
        ('*       (joy-eval (cdr p) (cons (* (cadr s) (car s)) (cddr s))))
        ('/       (joy-eval (cdr p) (cons (/ (cadr s) (car s)) (cddr s))))
        ('<       (joy-eval (cdr p) (cons (if (< (cadr s) (car s)) 'true 'false) (cddr s))))
        ('>       (joy-eval (cdr p) (cons (if (> (cadr s) (car s)) 'true 'false) (cddr s))))
        ('<=      (joy-eval (cdr p) (cons (if (<= (cadr s) (car s)) 'true 'false) (cddr s))))
        ('>=      (joy-eval (cdr p) (cons (if (>= (cadr s) (car s)) 'true 'false) (cddr s))))
        ('=       (joy-eval (cdr p) (cons (if (equal? (cadr s) (car s)) 'true 'false) (cddr s))))
        ('null    (joy-eval (cdr p) (cons (if (null? (car s)) 'true 'false) (cdr s))))
        ('list    (joy-eval (cdr p) (cons (if (or (pair? (car s)) (null? (car s)))
                                              'true 'false) (cdr s))))
        ('leaf    (joy-eval (cdr p) (cons (if (and (atom? (car s)) (not (null? (car s))))
                                              'true 'false) (cdr s))))
        ('pair    (joy-eval (cdr p) (cons (if (pair? (car s)) 'true 'false) (cdr s))))
        ('atom    (joy-eval (cdr p) (cons (if (atom? (car s)) 'true 'false) (cdr s))))
        ('not     (joy-eval (cdr p) (cons (if (equal? 'true (car s))
                                              'false 'true) (cdr s))))
        ('and     (joy-eval (cdr p) (cons (if (and (equal? 'true (cadr s))
                                                   (equal? 'true (car s)))
                                              'true 'false) (cddr s))))
        ('or      (joy-eval (cdr p) (cons (if (or (equal? 'true (cadr s))
                                                  (equal? 'true (car s)))
                                              'true 'false) (cddr s))))
        ('ifte    (joy-eval (cdr p) (if (equal? 'true (car (joy-eval (caddr s) (cdddr s))))
                                        (joy-eval (cadr s) (cdddr s))
                                        (joy-eval (car s) (cdddr s)))))
        ('intern  (joy-eval (cdr p) (cons (string->symbol (car s)) (cdr s))))
        ('rand    (joy-eval (cdr p) (cons (random (- (expt 2 2048) 1)) s)))
        ('div     (joy-eval (cdr p) (cons (mod (cadr s) (car s))
                                          (cons (div (cadr s) (car s)) (cdr (cdr s))))))
        (else     (joy-eval (cdr p) (cons (car p) s)))))
     ((null? p) s)
     (else (create-exception-state)))))

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

(define return-top-stack-element-only car)

(if (not (null? (command-line-arguments)))
    (call-with-string-output-port
     (lambda (p)
       (random-seed (time-nanosecond (current-time)))
       (pretty-print
        (return-top-stack-element-only
         (joy-eval
          (call-with-input-file (car (command-line-arguments)) read)
          '())) p)
       (display
        (joy-brackets
         (get-output-string p))))))
