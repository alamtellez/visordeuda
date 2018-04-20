#lang racket

(require "calculos.rkt")
; Declarar variables
(define units 450)
(define num_semestres_completo 18)
(define num_meses_completo (* num_semestres_completo 12))
(define porcentaje 0.25)
;Definir el costo del primer semestre a pagar
(define (costo_semestre inicial unidades_iniciales total_unidades)
  (*(/ inicial unidades_iniciales) (/ total_unidades 9)))
;Definir el costo del primer semestre
;(define semestre (costo_semestre ref_semestre ref_unidades units))
(define semestre 117047)
;Calcular el total que costará la carrera con intereses
(define total_c_intereses (* semestre 9 (+ 1 rate_anual)))
;Calcular el financiamiento correspondiente a el porcentaje por el total del costo de la carrera
(define financiamiento_inicial (* total_c_intereses porcentaje))
;Calcular el la deuda semestral de acuerdo al plazo al que se quiera ir el alumno 
(define deuda_semestral_inicial (/ financiamiento_inicial num_semestres_completo))
; Calcular el costo del semestre de acuerdo a las unidades de cada carrera
(define a_pagar_plazo_completo (a_pagar_fun (list deuda_semestral_inicial) rate_semestral num_semestres_completo))
(define pagos_plazo_completo (pagos_fun 108 '() a_pagar_plazo_completo rate_mensual 0))
(define todo_a_pagar_plazo_completo (todo_a_pagar_fun '() 108 a_pagar_plazo_completo rate_mensual pagos_plazo_completo 0))
(define interes_plazo_completo (interes_fun '() 108 a_pagar_plazo_completo todo_a_pagar_plazo_completo rate_mensual 0))
(define amortizacion_plazo_completo (amortizacion_fun '() interes_plazo_completo pagos_plazo_completo 108 0))
(define comportamiento_pagos_completo (acumulado '() pagos_plazo_completo 0 (length pagos_plazo_completo)))
(define comportamiento_amortización_completo (acumulado-inverso '() amortizacion_plazo_completo 0 (length amortizacion_plazo_completo)))


(define (print_values li res)
  (cond
    [(empty? li) res]
    [else (print_values (cdr li) (append res (list (format "~a ," (car li)))))]))