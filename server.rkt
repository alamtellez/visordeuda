#lang racket

(require web-server/servlet
         web-server/servlet-env)

(require net/uri-codec)
(require "calculos.rkt")

(define (start req)
  
  (define uri (request-uri req))
  (define path (map path/param-path (url-path uri)))    
  (define page (car path))
  
  (cond 
    
    ; /form
    [(equal? page "")
     (response/xexpr
   '(html
    (head
      (title "Deuda")
      (link ((type "text/css") (rel "stylesheet") (href "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/css/materialize.min.css")  (media "screen,projection"))))
    (body ([style "background-color: #000066;"])

    	(div ((class "container"))
    		(div ((class "card-panel z-depth-5") (style "margin-top: 90px"))
		      (div ((class "row"))
			      (div ((class "col s12 m12"))
		          (h4 ((class "center")) "Intoduce tus datos")
              (div ((class "row"))
                (form ([method "POST"] [action "/print-form-data"] [class "col s12 m12"])
                  (div ((class "row"))
                    (div ((class "input-field col s12 m4"))
                      (select ([name "units"])
                       (option ((value "") (selected "true") ) "Choose your option")
                            (option ([value "450"]) "IC")
                            (option ([value "498"]) "ARQ")
                            (option ([value "450"]) "IA")
                            (option ([value "446"]) "IBT")
                            (option ([value "450"]) "IDS")
                            (option ([value "450"]) "IIA")
                            (option ([value "450"]) "IIS")
                            (option ([value "450"]) "IMA")
                            (option ([value "450"]) "IMT")
                            (option ([value "446"]) "ISC")
                            (option ([value "450"]) "ISD")
                            (option ([value "438"]) "LAD")
                            (option ([value "438"]) "LAE")
                            (option ([value "438"]) "LAF")
                            (option ([value "450"]) "LCD")
                            (option ([value "450"]) "LDI")
                            (option ([value "438"]) "LEM")
                            (option ([value "438"]) "LIN")
                            (option ([value "438"]) "LRI")
                            )
                      (label "Carrera" )
                    )
                  
                    (div ((class "input-field col s12 m4"))
                      (i ((class "mdi-action-account-circle prefix")))
                      (input ((id "icon_") (type "number") (name "percentage") (min "0") (max "100")) )
                      (label ((for "icon_")) "Porcentaje de beca financiamiento (en enteros Ej. 50)")
                    )
                    (div ((class "input-field col s12 m4"))
                      (select ([name "meses"])
                       (option ((value "") (selected "true") ) "Choose your option")
                            (option ([value "12"]) "1 Año")
                            (option ([value "24"]) "2 Años")
                            (option ([value "36"]) "3 Años")
                            (option ([value "48"]) "4 Años")
                            (option ([value "60"]) "5 Años")
                            (option ([value "72"]) "6 Años")
                            (option ([value "84"]) "7 Años")
                            (option ([value "96"]) "8 Años")
                            (option ([value "108"]) "9 Años")
                            )
                      (label "¿A cuántos años?" )
                    )
                  )
                  (button ((class "btn waves-effect waves-light right") (type "submit")) "Enviar")
                )

              )
              
            )
            (div ((class "col s12 m12"))
            	(h2 ([class "center"])
                "¿Cuánto le deberás al tec?
                ¿Cómo harás para pagarle?"
            	)
              (img ([src  "http://www.quantumworks.io/img/burning_tec.png"] [alt ""] [class "responsive-img"]))
            )

          )
        )
	    )


      
      (script ((type "text/javascript") (src "https://code.jquery.com/jquery-2.1.1.min.js")))
      (script ((type "text/javascript") (src "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/js/materialize.min.js")))
      (script ((type "text/javascript")) "$(document).ready(function() {
$('select').material_select();
});")

    )
  ))]
    
    ; /print-form-data
    [(equal? page "print-form-data")
  
     ; extract the form data:
     (define post-data (bytes->string/utf-8 (request-post-data/raw req)))
     
     ; convert to an alist:
     (define form-data (form-urlencoded->alist post-data))

     ; pull out the user and comment:
     (define units (string->number(cdr (assq 'units form-data))))
     (define porcentaje (/ (string->number (cdr (assq 'percentage form-data))) 100))
     (define meses (string->number(cdr (assq 'meses form-data))))
     
     
     ; Declarar variables
     (define num_semestres_completo (/ meses 6))
     
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
     (define pagos_plazo_completo (pagos_fun meses '() a_pagar_plazo_completo rate_mensual 0))
     (define todo_a_pagar_plazo_completo (todo_a_pagar_fun '() meses a_pagar_plazo_completo rate_mensual pagos_plazo_completo 0))
     (define interes_plazo_completo (interes_fun '() meses a_pagar_plazo_completo todo_a_pagar_plazo_completo rate_mensual 0))
     (define amortizacion_plazo_completo (amortizacion_fun '() interes_plazo_completo pagos_plazo_completo meses 0))
     (define comportamiento_pagos_completo (acumulado '() pagos_plazo_completo 0 (length pagos_plazo_completo)))
     (define comportamiento_amortización_completo (acumulado-inverso '() amortizacion_plazo_completo 0 (length amortizacion_plazo_completo)))
     ; send back the extracted data:
     (response/xexpr
      `(html
        (head
         (title "Deuda")
         (link ((type "text/css") (rel "stylesheet") (href "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/css/materialize.min.css")  (media "screen,projection")))
        )
        (body ([style "background-color: #000066;"])
         
            (div ([class "row"])
               (div ([class  "card col s12 m12 center-align"])
                   (h5 "Tomando como referencia el costo de semestre de Ene-May 2018")
                   (h6 "Recién graduado habrás acumulado una deuda de: " ,(format "~a" financiamiento_inicial))
                   (h6 "Cumpliendo el plazo de ", (format "~a" (/ meses 12)), " años, habrás acumulado: " ,(format "~a" (last comportamiento_pagos_completo)), " para el final")
                   (h4 ([style "color: red;"]) "ESO ES " ,(format "~a" (*(/ (last comportamiento_pagos_completo) financiamiento_inicial) 100)), "% DEL COSTO INICIAL")
               )
              (div ([class  "col s12 m6"])
                 (div ([class "card"])
                   (canvas ([id "line-chart"]))
                   (p ([id "comportamiento_pagos_completo"] [hidden "true"]) " " ,(format "~a" comportamiento_pagos_completo))
                   (p ([id "comportamiento_amortizacion_completo"] [hidden "true"]) " " ,(format "~a" comportamiento_amortización_completo))
                   
                 )
               )
              (div ([class  "card col s12 m6"])
                   
                   (canvas ([id "line-chart2"]))
                   (p ([id "pagos_plazo_completo"] [hidden "true"]) " ",(format "~a" pagos_plazo_completo))
               )
              (div ([class  "card col s12 m12 center-align"])
                   
                   (h4 ([style "color:  #000066;"]) "Recuerda que el tec nunca pierde")
               )
              
            )
         
         
         (script ((type "text/javascript") (src "https://code.jquery.com/jquery-2.1.1.min.js")))
         (script ((type "text/javascript") (src "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/js/materialize.min.js")))
         (script ((type "text/javascript") (src "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.4.0/Chart.min.js")))
         (script ((type "text/javascript")) "
var string_pagos = document.getElementById('comportamiento_pagos_completo').innerHTML;
  var string_pagos = string_pagos.substring(2, string_pagos.length-1);


  var array_pagos = string_pagos.split(' ');

  var string_amortizacion = document.getElementById('comportamiento_amortizacion_completo').innerHTML;
  var string_amortizacion = string_amortizacion.substring(2, string_amortizacion.length-1);

  var array_amortizacion = string_amortizacion.split(' ');

  var string_pagos_tiempo = document.getElementById('pagos_plazo_completo').innerHTML;
  var string_pagos_tiempo = string_pagos_tiempo.substring(2, string_pagos_tiempo.length-1);


  var array_pagos_tiempo = string_pagos_tiempo.split(' ');

  var indexes = [array_pagos.length];
  var i = 1;
  for(el in array_pagos){
    indexes[i] = i;
    i++;
  }

  new Chart(document.getElementById('line-chart'), {
    type: 'line',
    data: {
      labels: indexes,
      datasets: [{
          data: array_amortizacion,
          label: 'Amortización',
          borderColor: '#3e95cd',
          fill: false
        }, {
          data: array_pagos,
          label: 'Aumento costo crédito',
          borderColor: '#8e5ea2',
          fill: false
        }
      ]
    },
    options: {
      title: {
        display: true,
        text: 'Aumento del crédito vs amortización'
      },
      scales: {
          yAxes: [{
            scaleLabel: {
                  display: true,
                  labelString: 'Costo'
            }
          }],
          xAxes: [{
            scaleLabel: {
                  display: true,
                  labelString: 'Meses'
            }
          }]
      }
    }
  });

  new Chart(document.getElementById('line-chart2'), {
    type: 'line',
    data: {
      labels: indexes,
      datasets: [{
          data: array_pagos_tiempo,
          label: 'Pagos con el tiempo',
          borderColor: '#3e95cd',
          fill: false
        }
      ]
    },
    options: {
      title: {
        display: true,
        text: 'Visualización de pagos a realizar con el tiempo en el periodo'
      },
      scales: {
          yAxes: [{
            scaleLabel: {
                  display: true,
                  labelString: 'Costo'
            }
          }],
          xAxes: [{
            scaleLabel: {
                  display: true,
                  labelString: 'Meses'
            }
          }]
      }
    }
  });
")
         )))]
    
    ; another page?
    [else
     (response/xexpr
      `(html
        (body
         (p "Page not found!"))))]))
         

(define port (if (getenv "PORT")
                 (string->number (getenv "PORT"))
                 8080))
(serve/servlet start
               #:servlet-regexp #rx""
               #:servlet-path "/"
               #:listen-ip #f
               #:port port
               #:command-line? #t
               )
