#lang racket

(require "datatypes.rkt")
(require (lib "eopl.ss" "eopl"))
(require "eopl_errors.rkt")

(define check-type (lambda (sttmnts) (type-of-statements sttmnts)))

(define type-of-statements (lambda (sttmnts) (cases statements sttmnts
                                                    (a-statement (sttmnt) (type-of-statement sttmnt))
                                                    (some-statements (sttmnts sttmnt) (let ((res (type-of-statements sttmnts)))
                                                                                      (if (equal? res none-type) (type-of-statement sttmnt) res))))))

(define type-of-statement (lambda (sttmnt) (cases statement sttmnt
                                                  (compound-statement (cmp) (type-of-compound cmp))
                                                  (simple-statement (smpl) (type-of-simple smpl)))))

(define type-of-simple (lambda (smpl) (cases simple smpl
                                             (assignment-statement (assign) (type-of-assignment assign))
                                             (global-statement (glbl) (type-of-global glbl))
                                             (return-statement (rtrn) (type-of-return rtrn))
                                             (pass-statement () (none-type))
                                             (break-statement () (none-type))
                                             (continue-statement () (none-type)))))

(define type-of-compound (lambda (cmp) (cases compound cmp
                                              (function-definition (func) (type-of-function-def func))
                                              (if-statement (if-stmt) (type-of-if if-stmt))
                                              (for-statement (for-stmt) (type-of-for for-stmt)))))

(define type-of-assignment (lambda (assign) (cases assignment assign
                                                   (an-assignment (var exp) (let ((res1 (type-of-ATOM var)) (res2 (type-of-expression exp)))
                                                                              (if (equal? res1 res2) (none-type) (report-wrong-type! (type->string res1) res2
                                                                                    (string-append (var->string var) " = " (expression->string exp)))))))))

(define type-of-global (lambda (glbl) (cases global glbl
                                                           (a-global (var) (none-type)))))

(define type-of-return (lambda (rtrn) (cases return rtrn
                                                           (void-return () (none-type))
                                                           (exp-return (exp) (type-of-expression exp)))))

(define type-of-function-def (none-type))

(define type-of-for (none-exp))

(define type-of-if (none-exp))

(define type-of-expression (lambda (exp) (cases expression exp
                                                              (an-expression (dis) (type-of-disjunction dis)))))

(define type-of-disjunction (lambda (dis) (cases disjunction dis
                                                                (a-disjunction (disj con) (let ((res1 (type-of-disjunction disj))
                                                                                               (res2 (type-of-conjunction con)))
                                                                                           (if (equal? res1 bool-type) (if (equal? res2 bool-type) (bool-type)
                                                                                           (report-wrong-type! 'bool res2 (disjunction->string dis)))
                                                                                           (report-wrong-type! 'bool res1 (disjunction->string dis)))))
                                                                (conjunction-exp (con) (type-of-conjunction con)))))

(define type-of-conjunction (lambda (con) (cases conjunction con
                                                               (a-conjunction (conj inv) (let ((res1 (type-of-conjunction conj))
                                                                                               (res2 (type-of-inversion inv)))
                                                                                          (if (equal? res1 bool-type) (if (equal? res2 bool-type) (bool-type)
                                                                                          (report-wrong-type! 'bool res2 (conjunction->string con)))
                                                                                          (report-wrong-type! 'bool res1 (conjunction->string con)))))
                                                               (inversion-exp (inv) (type-of-inversion inv)))))

(define type-of-inversion (lambda (inv) (cases inversion inv
                                                             (an-inversion (inve) (let ((res (type-of-inversion inve)))
                                                                                 (if (equal? res bool-type) (bool-type) (report-wrong-type! 'bool res
                                                                                  (inversion->string inv)))))
                                                             (comp-exp (comp) (type-of-comparison comp)))))

(define type-of-comparison (lambda (comp) (cases comparison comp
                                                               (equal-sum (eq) (type-of-eq eq))
                                                               (lessthan-sum (lt) (type-of-lt lt))
                                                               (lessthanorequal-sum (let-e) (type-of-let let-e))
                                                               (greaterthan-sum (gt) (type-of-gt gt))
                                                               (greaterthanorequal-sum (get) (type-of-get get))
                                                               (sum-expression (sum) (type-of-sum sum)))))

(define type-of-eq (lambda (eq) (cases eq-exp eq
                                                     (an-eq-exp (num1 num2) (let ((res1 (type-of-sum num1))
                                                                                  (res2 (type-of-sum num2)))
                                                                              (if (equal? res1 int-type)
                                                                                (if (equal? res2 int-type) (int-type)
                                                                                (if (equal? res2 float-type) (float-type)
                                                                                    (report-wrong-type! 'int res2 (eq-exp->string eq))))
                                                                                (if (equal? res1 float-type)
                                                                                    (if (equal? res2 int-type) (float-type)
                                                                                    (if (equal? res2 float-type) (float-type)
                                                                                        (report-wrong-type! 'float res2 (eq-exp->string eq))))
                                                                                    (report-wrong-type! 'int res1 (eq-exp->string eq)))))))))

(define type-of-lt (lambda (lt) (cases lt-exp lt
                                                     (a-lt-exp (num1 num2) (let ((res1 (type-of-sum num1))
                                                                                  (res2 (type-of-sum num2)))
                                                                              (if (equal? res1 int-type)
                                                                                (if (equal? res2 int-type) (int-type)
                                                                                (if (equal? res2 float-type) (float-type)
                                                                                    (report-wrong-type! 'int res2 (lt-exp->string lt))))
                                                                                (if (equal? res1 float-type)
                                                                                    (if (equal? res2 int-type) (float-type)
                                                                                    (if (equal? res2 float-type) (float-type)
                                                                                        (report-wrong-type! 'float res2 (lt-exp->string lt))))
                                                                                    (report-wrong-type! 'int res1 (lt-exp->string lt)))))))))

(define type-of-gt (lambda (gt) (cases gt-exp gt
                                                     (a-gt-exp (num1 num2) (let ((res1 (type-of-sum num1))
                                                                                  (res2 (type-of-sum num2)))
                                                                              (if (equal? res1 int-type)
                                                                                (if (equal? res2 int-type) (int-type)
                                                                                (if (equal? res2 float-type) (float-type)
                                                                                    (report-wrong-type! 'int res2 (gt-exp->string gt))))
                                                                                (if (equal? res1 float-type)
                                                                                    (if (equal? res2 int-type) (float-type)
                                                                                    (if (equal? res2 float-type) (float-type)
                                                                                        (report-wrong-type! 'float res2 (gt-exp->string gt))))
                                                                                    (report-wrong-type! 'int res1 (gt-exp->string gt)))))))))

(define type-of-let (lambda (lte) (cases let-exp lte
                                                     (a-let-exp (num1 num2) (let ((res1 (type-of-sum num1))
                                                                                  (res2 (type-of-sum num2)))
                                                                              (if (equal? res1 int-type)
                                                                                (if (equal? res2 int-type) (int-type)
                                                                                (if (equal? res2 float-type) (float-type)
                                                                                    (report-wrong-type! 'int res2 (let-exp->string lte))))
                                                                                (if (equal? res1 float-type)
                                                                                    (if (equal? res2 int-type) (float-type)
                                                                                    (if (equal? res2 float-type) (float-type)
                                                                                        (report-wrong-type! 'float res2 (let-exp->string lte))))
                                                                                    (report-wrong-type! 'int res1 (let-exp->string lte)))))))))

(define type-of-get (lambda (get) (cases get-exp get
                                                     (a-get-exp (num1 num2) (let ((res1 (type-of-sum num1))
                                                                                  (res2 (type-of-sum num2)))
                                                                              (if (equal? res1 int-type)
                                                                                (if (equal? res2 int-type) (int-type)
                                                                                (if (equal? res2 float-type) (float-type)
                                                                                    (report-wrong-type! 'int res2 (get-exp->string get))))
                                                                                (if (equal? res1 float-type)
                                                                                    (if (equal? res2 int-type) (float-type)
                                                                                    (if (equal? res2 float-type) (float-type)
                                                                                        (report-wrong-type! 'float res2 (get-exp->string get))))
                                                                                    (report-wrong-type! 'int res1 (get-exp->string get)))))))))

(define type-of-sum (lambda  (sum) (cases sum-exp sum
                                                        (plus-term (num1 num2) (let ((res1 (type-of-sum num1))
                                                                                     (res2 (type-of-term num2)))
                                                                                 (if (equal? res1 int-type) (if (equal? res2 int-type) (int-type)
                                                                                   (if (equal? res2 float-type) (float-type)
                                                                                       (report-wrong-type! 'int res2 (sum->string sum))))
                                                                                 (if (equal? res1 float-type) (if (equal? res2 int-type) (float-type)
                                                                                   (if (equal? res2 float-type) (float-type)
                                                                                       (report-wrong-type! 'float res2 (sum->string sum))))
                                                                                 (if (equal? res1 list-type) (if (equal? res2 list-type) (list-type)
                                                                                   (report-wrong-type! 'list res2 (sum->string sum)))
                                                                                 (report-wrong-type! 'float res1 (sum->string sum)))))))
                                                        (minus-term (num1 num2) (let ((res1 (type-of-sum num1))
                                                                                      (res2 (type-of-term num2)))
                                                                                  (if (equal? res1 int-type) (if (equal? res2 int-type) (int-type)
                                                                                    (if (equal? res2 float-type) (float-type)
                                                                                        (report-wrong-type! 'int res2 (sum->string sum))))
                                                                                  (if (equal? res1 float-type) (if
                                                                                    (or (equal? res2 int-type) (equal? res2 float-type)) (float-type)
                                                                                  (report-wrong-type! 'float res2 (sum->string sum)))
                                                                                      (report-wrong-type! 'float res1 (sum->string sum))))))
                                                        (term-expression (term) (type-of-term term)))))

(define type-of-term (lambda (term) (cases term-exp term
                                                         (times-factor (num1 num2) (let ((res1 (type-of-term num1))
                                                                                         (res2 (type-of-factor num2)))
                                                                                     (if (equal? res1 int-type) (if (equal? res2 int-type) (int-type)
                                                                                    (if (equal? res2 float-type) (float-type)
                                                                                        (report-wrong-type! 'int res2 (term->string term))))
                                                                                  (if (equal? res1 float-type) (if
                                                                                    (or (equal? res2 int-type) (equal? res2 float-type)) (float-type)
                                                                                  (report-wrong-type! 'float res2 (term->string term)))
                                                                                      (report-wrong-type! 'float res1 (term->string term))))))
                                                         (divides-factor (num1 num2) (let ((res1 (type-of-term num1))
                                                                                           (res2 (type-of-factor num2)))
                                                                                       (if (equal? res1 int-type) (if (equal? res2 int-type) (int-type)
                                                                                    (if (equal? res2 float-type) (float-type)
                                                                                        (report-wrong-type! 'int res2 (term->string term))))
                                                                                  (if (equal? res1 float-type) (if
                                                                                    (or (equal? res2 int-type) (equal? res2 float-type)) (float-type)
                                                                                  (report-wrong-type! 'float res2 (term->string term)))
                                                                                      (report-wrong-type! 'float res1 (term->string term))))))
                                                         (factor-expression (factor) (type-of-factor factor)))))

(define type-of-factor (lambda (factor) (cases factor-exp factor
                                                             (plus-power (pow) (type-of-power pow))
                                                             (minus-power (pow) (type-of-power pow))
                                                             (power-expression (pow) (type-of-power pow)))))

(define type-of-power (lambda (pow) (cases power-exp pow
                                                         (pow-exp (num1 num2) (let ((res1 (type-of-ATOM num1))
                                                                                    (res2 (type-of-factor num2)))
                                                                                (if (equal? res1 int-type) (if (equal? res2 int-type) (int-type)
                                                                                    (if (equal? res2 float-type) (float-type)
                                                                                        (report-wrong-type! 'int res2 (power->string pow))))
                                                                                  (if (equal? res1 float-type) (if
                                                                                    (or (equal? res2 int-type) (equal? res2 float-type)) (float-type)
                                                                                  (report-wrong-type! 'float res2 (power->string pow)))
                                                                                      (report-wrong-type! 'float res1 (power->string pow))))))
                                                         (primary-expression (prim) (type-of-primary prim)))))

(define type-of-primary (lambda (primary) (cases primary-exp primary
                                                               (atom-exp (var) (type-of-ATOM var))
                                                               (list-idx (prim exp) (let ((res1 (type-of-primary prim)) (res2 (type-of-expression exp)))
                                                                                      (if (equal? res1 list-type) (if (equal? res2 int-type) (none-type)
                                                                                          (report-wrong-type! 'int res2 (primary->string primary)))
                                                                                          (report-wrong-type! 'list res1 (primary->string primary)))))
                                                               (func-call (prim args) (none-type))
                                                               (func-call-noargs (prim) (none-type)))))


(define type-of-ATOM (lambda (atom) (cases ATOM atom
                                                         (id-exp (name ty) ty)
                                                         (num-exp (num) (cases expval num
                                                                          (num-val (number) (if (integer? number) (int-type) (float-type)))
                                                                          (else none-type)))
                                                         (list-expression (l) (list-type))
                                                         (true-exp () (bool-type))
                                                         (false-exp () (bool-type))
                                                         (none-exp () (none-type)))))

(provide (all-defined-out))