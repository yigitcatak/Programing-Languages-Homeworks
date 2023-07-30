(define get-operator (lambda (op-symbol)
  (cond
    ((eq? op-symbol '+) +)
    ((eq? op-symbol '*) *)
    ((eq? op-symbol '-) -)
    ((eq? op-symbol '/) /)
    ((eq? op-symbol '=) =)
    ((eq? op-symbol '<) <)
    ((eq? op-symbol '>) >)
    (else 'error))))

(define get-value (lambda (var env)
    (cond 
       ( (null? env) 'error)
       ( (eq? var (caar env)) (cdar env))
       ( else (get-value var (cdr env))))))

(define extend-env (lambda (var val old-env)
        (cons (cons var val) old-env)))

(define define-expr? (lambda (e)
         (and (list? e) (= (length e) 3) (eq? (car e) 'define) (symbol?(cadr e)))))

(define if-expr? (lambda (e)
         (and (list? e) (= (length e) 4) (eq? (car e) 'if))))

(define cond-expr? (lambda (e)
   (cond
      ((not (list? e)) #f)
      ((and (eq? (car e) 'cond) (> (length e) 2)) (check-cond (cdr e)))
      ((eq? (car e) 'cond) 'error)
      (else #f)
   )
))

(define check-cond (lambda (e)
      (cond
      ( (null? e) 'error)
      ( (not (and (list? (car e)) (= (length (car e)) 2))) 'error)
      ( (and (eq? (caar e) 'else) (> (length e) 1)) 'error)
      ( (eq? (caar e) 'else) #t)
      ( else (check-cond (cdr e))))))

(define eval-if (lambda (e env)
     (not (= (eval-value e env) 0))))

(define eval-cond (lambda (e env)
   (cond 
      ((eq? (caar e) 'else) (cadar e))
      ((eval-if (caar e) env) (eval-value (cadar e) env))
      (else (eval-cond (cdr e) env))
   )
))

(define eval-value (lambda (e env)
   (cond
      ( (number? e) e)
      ( (symbol? e) (get-value e env))
      ( (not (list? e)) 'error)
      ( (eq? (get-operator (car e)) 'error) 'error)
      ( else 
         (let (
               (operator (get-operator (car e)))
               (operands (map eval-value (cdr e) (make-list (length (cdr e) ) env )))
            )  
               (if (contains-error operands)
                  'error
                  (apply operator operands)
               )
            )))))

(define contains-error (lambda (list)
	(cond 
      ((null? list) #f)
	   ((eq? (car list) 'error) #t)
		(else (contains-error (cdr list))))))

(define repl (lambda (env)
   (let* (
            (dummy1 (display "cs305> "))
            (in-expr (read))

            (expr
               (cond
                  ((eq? (cond-expr? in-expr) 'error) 'error)
                  ((cond-expr? in-expr) 'cond)
                  (else 
                     (if (if-expr? in-expr)
                        (if (eval-if (cadr in-expr) env)
                           (caddr in-expr)
                           (cadddr in-expr))
                        in-expr
                      )
                  )
               )
            )
            
            (val (cond 
                        ((eq? expr 'cond) (eval-cond (cdr in-expr) env))
                        ((eq? expr 'error) 'error)
                        ((define-expr? expr) (cadr expr))
                        (else (eval-value expr env))
                     ))

            (new-env (cond
                        ((and (not (eq? val 'error)) (define-expr? expr)) (extend-env (cadr expr) (eval-value (caddr expr) env) env))
                        (else env)
                     ))

            (dummy2 (display "cs305: "))
            (dummy3 (if (eq? val 'error)
               (display "ERROR")
               (display val)
            ))
            
            (dummy4 (newline))
            (dummy5 (newline))
         )
         (repl new-env))))