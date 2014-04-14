; ==================
;    DEFGLOBAL
; ==================
(defglobal
   ?*corregido* = 0
   ?*mensaje* = ""
)

; ==================
;    DEFTEMPLATE
; ==================
(deftemplate EQUIPO
   (slot v_tipo
      (type SYMBOL)
   )
   (slot v_enciende
      (type SYMBOL)
   )
   (slot v_bateria_funciona
      (type SYMBOL)
   )
   (slot v_bateria_sin_carga
      (type SYMBOL)
   )
)

; ==================
;    INITIAL-FACT
; ==================
(defrule ARRANQUE
   (initial-fact)
=>
   (printout t "=================================================================" crlf)
   (printout t "BIENVENIDO Y GRACIAS POR USAR NUESTRO SISTEMA" crlf)
   (printout t "=================================================================" crlf)
   (printout t "ESCRIBE UNA PEQUENA DESCRIPCION DEL MALESTAR DE TU EQUIPO:" crlf)
   (read)
   (printout t "=================================================================" crlf)
   (printout t "GRACIAS, AHORA AYUDAME CONTESTANDO ALGUNAS PREGUNTAS:" crlf)
   (assert (arranque))
)

(defrule INICIO
   (arranque)
=>
   (printout t "DE QUE TIPO ES TU PC? (Portatil/Escritorio)" crlf)
   (assert (TIPO-EQUIPO (read)))
)

(defrule LLENA-DATOS-EQUIPO
   (TIPO-EQUIPO ?pTipo)
=>
   (printout t "TU EQUIPO ENCIENDE? (Si/No)" crlf)
   (assert (EQUIPO (v_tipo ?pTipo) (v_enciende (read))))
)

(defrule BATERIA-FUNCIONA
   ?hecho_actual <- (EQUIPO (v_tipo Portatil) (v_enciende No))
=>
   (printout t "AUN FUNCIONA LA BATERIA DE SU PORTATIL (Si/No)" crlf)
   (modify ?hecho_actual (v_bateria_funciona (read)))
)

(defrule BATERIA-CON-CARGA
   ?hecho_actual <- (EQUIPO (v_tipo Portatil) (v_enciende No) (v_bateria_funciona Si))
=>
   (printout t "EL INDICADOR DE BATERIA EN SU PORTATIL DA LA SENAL DE FALTA DE CARGA? (Si/No)" crlf)
   (modify ?hecho_actual (v_bateria_sin_carga (read)))
)

(defrule INDICADOR-BAJO
	?hecho_actual <- (EQUIPO (v_tipo Portatil) (v_enciende No) (v_bateria_funciona Si) (v_bateria_sin_carga Si))
=>
   (printout t "TIENE CONECTADO EL CARGADOR EN SU PORTATIL? (Si/No)" crlf)
   (assert (cargador-conectado (read)))
)

(defrule CARGADOR-CONECTADO
   ?hecho_actual <- (EQUIPO (v_tipo Portatil) (v_enciende No) (v_bateria_funciona Si) (v_bateria_sin_carga Si))
   (cargador-conectado No)
=>
   (printout t "R// POSIBLE CAUSA, REQUERIA CARGADOR" crlf)
   (printout t "CONECTE CARGADOR E INTENTE DE NUEVO" crlf)
;   (bind ?*corregido* 1)
   (assert (CORREGIDO Si))
   (bind ?*mensaje* "EL EQUIPO REQUERIA CARGADOR")
   (retract 5)
   (retract 2)
   (retract 1)
   (assert (arranque))
)

(defrule EQUIPO-ENCIENDE
   (EQUIPO (v_enciende Si))
   (CORREGIDO Si)
;   (CORREGIDO ?corregido&:(= 1 ?*corregido*))
=>
   (printout t ?*mensaje* crlf)
   (printout t "GRACIAS POR USAR NUESTRO SISTEMA" crlf "SALUDOS" crlf)
   (printout t "=================================================================" crlf)
)