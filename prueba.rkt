#lang racket

(require web-server/servlet
         web-server/servlet-env)

(require net/uri-codec)

(require "calculos.rkt")

(define (form-servlet req)
  
  (define uri (request-uri req))
  (define path (map path/param-path (url-path uri)))    
  (define page (car path))
  
  (cond 
    
    ; /form
    [(equal? page "form")
     (response/xexpr
      `(html
        (body
         (form ([method "POST"] [action "/print-form-data"])
               "user name: " (input ([type "number"] [name "units"]))
               (br)
               "comment: " (input ([type "text"] [name "porcentaje"]))
               (br)
               (input ([type "submit"]))))))]
    
    ; /print-form-data
    [(equal? page "print-form-data")
  
     ; extract the form data:
     (define post-data (bytes->string/utf-8 (request-post-data/raw req)))
     
     ; convert to an alist:
     (define form-data (form-urlencoded->alist post-data))

     ; pull out the user and comment:
     (define units (string->number(cdr (assq 'units form-data))))
     (define porcentaje (/ (string->number (cdr (assq 'porcentaje form-data))) 100))
     ;(printf "~a as a units is \n" units)
     ;(printf "~a as a porcentaje is \n" (* porcentaje units))
     
     
     ; Declarar variables
     ;(define units 450)
     (define num_semestres_completo 18)
     (define num_meses_completo (* num_semestres_completo 12))
     ;(define porcentaje 0.25)
     ;Definir el costo del primer semestre a pagar
     (define (costo_semestre inicial unidades_iniciales total_unidades)
       (*(/ inicial unidades_iniciales) (/ total_unidades 9)))
     ;Definir el costo del primer semestre
     (define semestre (costo_semestre ref_semestre ref_unidades units))

     ;Calcular el total que costará la carrera con intereses
     (define total_c_intereses (* semestre 9 (+ 1 rate_anual)))
     ;Calcular el financiamiento correspondiente a el porcentaje por el total del costo de la carrera
     (define financiamiento_inicial (* total_c_intereses porcentaje))
     ;Calcular el la deuda semestral de acuerdo al plazo al que se quiera ir el alumno 
     (define deuda_semestral_inicial (/ financiamiento_inicial num_semestres_completo))
     ; Calcular el costo del semestre de acuerdo a las unidades de cada carrera
     (define a_pagar_plazo_completo (a_pagar_fun (list deuda_semestral_inicial) rate_semestral num_semestres_completo))
     ;(printf "~a\n" a_pagar_plazo_completo)
     (define pagos_plazo_completo (pagos_fun 108 '() a_pagar_plazo_completo rate_mensual 0))
     (define todo_a_pagar_plazo_completo (todo_a_pagar_fun '() 108 a_pagar_plazo_completo rate_mensual pagos_plazo_completo 0))
     (define interes_plazo_completo (interes_fun '() 108 a_pagar_plazo_completo todo_a_pagar_plazo_completo rate_mensual 0))
     (define amortizacion_plazo_completo (amortizacion_fun '() interes_plazo_completo pagos_plazo_completo 108 0))
     (define comportamiento_pagos_completo (acumulado '() pagos_plazo_completo 0 (length pagos_plazo_completo)))
     (define comportamiento_amortización_completo (acumulado-inverso '() amortizacion_plazo_completo 0 (length amortizacion_plazo_completo)))
     (printf "~a" comportamiento_amortización_completo)
     
     ; send back the extracted data:
     (response/xexpr
      `(html
        (body
         (p ([hidden "true"]) " " ,(format "~a" comportamiento_pagos_completo))
         )))]
    
    ; another page?
    [else
     (response/xexpr
      `(html
        (body
         (p "Page not found!"))))]))
         
(serve/servlet form-servlet
               #:servlet-regexp #rx""
               #:servlet-path "/form")