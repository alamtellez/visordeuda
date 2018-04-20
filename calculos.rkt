#lang racket

;Declaradas aquí
(define ref_semestre 116132)
(define ref_unidades 48)
(define rate_anual 0.085)
(define rate_semestral (/ rate_anual 2))
(define rate_mensual (/ rate_anual 12))

; A_pagar pagos por numero de semestres
(define (a_pagar_fun li rate acc)
  (cond
    [(= acc 1) li]
    [else (a_pagar_fun (append li (list (* (last li) (+ 1 rate)))) rate (- acc 1))]))


;Calculo de pagos con intereses
(define (pagos_fun meses li a_pagar rate acc)
  (cond
    [(= acc meses) li]
    [(= acc 0) (pagos_fun meses (append li (list (calc_pago rate 6 (car a_pagar)))) a_pagar rate (+ acc 1))]
    [(= (modulo acc 6) 0) (pagos_fun meses (append li (list (calc_pago rate 6 (car (cdr a_pagar))))) (cdr a_pagar) rate (+ acc 1))]
    [else (pagos_fun meses (append li (list (calc_pago rate 6 (car a_pagar)))) a_pagar rate (+ acc 1))]))
;Formula pago
(define (calc_pago rate periodo total)
  (/ (* total rate) (- 1 (expt (+ 1 rate) (* -1 periodo)))))

;Funcion que calcula todos los pagos a realizar
(define (todo_a_pagar_fun li meses a_pagar_arg rate pagos_arg acc)
  (cond
    [(= acc meses) li]
    [(= (modulo acc 6) 0) (todo_a_pagar_fun (append li (list (- (car a_pagar_arg) (- (car pagos_arg) (* (car a_pagar_arg) rate))))) meses (cdr a_pagar_arg) rate (cdr pagos_arg) (+ acc 1))]
    [else
     (cond
       [(< (- (last li) (- (car pagos_arg) (* (last li) rate))) 3e-10)
        (todo_a_pagar_fun (append li (list 0)) meses a_pagar_arg rate (cdr pagos_arg) (+ acc 1))]
       [else (todo_a_pagar_fun (append li (list (- (last li) (- (car pagos_arg) (* (last li) rate))))) meses a_pagar_arg rate (cdr pagos_arg) (+ acc 1))])]))


(define (interes_fun li meses a_pagar_arg todo_a_pagar_arg rate acc)
  (cond
    [(= acc meses) li]
    [(= acc 0) (interes_fun (append li (list (* (car a_pagar_arg) rate))) meses (cdr a_pagar_arg) todo_a_pagar_arg rate (+ acc 1))]
    [(= (modulo acc 6) 0) (interes_fun (append li (list (* (car a_pagar_arg) rate))) meses (cdr a_pagar_arg) (cdr todo_a_pagar_arg) rate (+ acc 1))]
    [else (interes_fun (append li (list (* (car todo_a_pagar_arg) rate))) meses a_pagar_arg (cdr todo_a_pagar_arg) rate (+ acc 1))]))



(define (amortizacion_fun li interes_arg pagos_arg meses acc)
  (cond
    [(= acc meses) li]
    [else (amortizacion_fun (append li (list (- (car pagos_arg) (car interes_arg)))) (cdr interes_arg) (cdr pagos_arg) meses (+ acc 1))]))

(define reduce
  (λ (f init ls)
    (if (empty? ls)
        init
        (reduce f (f init (first ls)) (rest ls)))))

(define (acumulado li origen acc n)
  (cond
    [(= acc n) li]
    [(= acc 0) (acumulado (append li (list (car origen))) (cdr origen) (+ acc 1) n)]
    [else (acumulado (append li (list (+ (car origen) (last li)))) (cdr origen) (+ acc 1) n)]))

(define (acumulado-inverso li origen acc n)
  (cond
    [(= acc n) li]
    [(= acc 0) (acumulado-inverso (cons (car origen) li) (cdr origen) (+ acc 1) n)]
    [else (acumulado-inverso (cons (+ (car origen) (car li)) li) (cdr origen) (+ acc 1) n)]))

(provide (all-defined-out))


