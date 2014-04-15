; ==================
;    DEFGLOBAL
; ==================
(defglobal
   ?*corregido* = 0
   ?*mensaje* = ""
   ?*localvar1* = ""
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

(deftemplate EXTENSION
   (slot v_tipo
      (type SYMBOL)
;	  (allowed-symbols Extension Regleta)
   )
   (slot v_boton_ON
      (type SYMBOL)
;	  (allowed-symbols Si No)
   )
   (slot v_existe
      (type SYMBOL)
;	  (allowed-symbols Si No)
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

; =====================
;    PREGUNTA INICIAL
; =====================
(defrule INICIO
   (arranque)
=>
   (printout t "DE QUE TIPO ES TU PC? (Portatil/Escritorio)" crlf)
   (bind ?*localvar1* (read))
   (printout t "TU EQUIPO ENCIENDE? (Si/No)" crlf)
   (assert (EQUIPO (v_tipo ?*localvar1*) (v_enciende (read))))
   (assert (bateria-funciona))
)

; ===========================
;    EN CASO DE SER LAPTOP
; ===========================
(defrule BATERIA-FUNCIONA
   ?hecho <- (bateria-funciona)
   ?hecho_actual <- (EQUIPO (v_tipo Portatil) (v_enciende No))
=>
   (printout t "AUN FUNCIONA LA BATERIA DE SU PORTATIL (Si/No)" crlf)
   (bind ?valor (read))
   (if (eq ?valor Si)
      then
         (modify ?hecho_actual (v_bateria_funciona Si))
         (assert (bateria-carga))
	  else
	     (modify ?hecho_actual (v_bateria_funciona No))
		 (printout t "RECOMENDACION: RECOMENDAMOS COMPRAR UNA BATERIA NUEVA" crlf)
		 (assert (valida-extension))
   )
   (retract ?hecho)
)

(defrule BATERIA-CON-CARGA
   ?hecho <- (bateria-carga)
   ?hecho_actual <- (EQUIPO (v_tipo Portatil) (v_enciende No) (v_bateria_funciona Si))
=>
   (printout t "EL INDICADOR DE BATERIA EN SU PORTATIL DA LA SENAL DE FALTA DE CARGA? (Si/No)" crlf)
   (bind ?valor (read))
   (if (eq ?valor Si)
      then
	     (modify ?hecho_actual (v_bateria_sin_carga Si))
		 (assert (indicador-bajo))
	  else
	     (modify ?hecho_actual (v_bateria_sin_carga No))
		 (assert (valida-extension))
   )
   (retract ?hecho)
)

(defrule INDICADOR-BAJO
   ?hecho <- (indicador-bajo)
   ?hecho_actual <- (EQUIPO (v_tipo Portatil) (v_enciende No) (v_bateria_funciona Si) (v_bateria_sin_carga Si))
=>
   (printout t "TIENE CONECTADO EL CARGADOR EN SU PORTATIL? (Si/No)" crlf)
   (bind ?valor (read))
   (if (eq ?valor No)
      then
	     (assert (cargador-conectado No))
		 (assert (cargador-no-conectado))
      else
	     (assert (cargador-conectado Si))
		 (assert (valida-extension))
   )
   (retract ?hecho)
)

; =====================================
;    PROBLEMAS DE CORRIENTE ELECTRICA
; =====================================
(defrule UTILIZA-EXTENSION "Validacion para iniciar las verificaciones electricas"
   ?hecho <- (valida-extension)
=>
   (printout t "UTILIZA ALGUNA EXTENSION O REGLETA (Si/No)" crlf)
   (bind ?existe (read))
   (if (eq ?existe Si)
      then
         (printout t "QUE TIPO DE EXTENSION USA (Extension/Regleta)" crlf)
	     (bind ?tipo (read))
	     (assert (EXTENSION (v_existe ?existe) (v_tipo ?tipo)))
	     (assert (extension-bien-conectada))
	  else
	     (assert (verifique-cable-poder))
   )
   (retract ?hecho)
)

(defrule EXTENSION-BIEN-CONECTADA
   ?hecho <- (extension-bien-conectada)
   ?hecho_extension <- (EXTENSION (v_tipo ?tipo))
=>
   (printout t "VERIFIQUE QUE LA EXTENSION ESTE BIEN CONECTADA, ¿LO ESTA? (Si/No)" crlf)
   (bind ?conectado (read))
   (if (eq ?conectado No)
      then
	     (assert (regleta_bien_conectada No))
	  else
	     (if (eq ?tipo Regleta)
		    then
			   (assert (regleta_encendida))
		 )
   )
   (retract ?hecho)
)

(defrule EXTENSION-MAL-CONECTADA
   ?hecho <- (regleta_bien_conectada No)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
=>
   (printout t "R// POSIBLE CAUSA, EXTENSION MAL CONECTADA" crlf)
   (printout t "CONECTE BIEN SU REGLETA E INTENTE DE NUEVO" crlf)
;   (bind ?*corregido* 1)
   (assert (CORREGIDO Si))
   (bind ?*mensaje* "LA EXTENSION O REGLETA ESTABA MAL CONECTADA")
   (retract ?hecho_actual)
   (retract 1)
   (retract ?hecho)
   (assert (arranque))
)

(defrule REGLETA-ENCENDIDA ;regla para validar que la regleta este encendida
   (EXTENSION (v_existe Si) (v_tipo Regleta))
=>
   (printout t "VERIFIQUE QUE LA REGLETA ESTE ACTIVA <ON> ¿LO ESTA?" crlf)
   (assert (regleta_encendida (read)))
)

(defrule REGLETA-APAGADA
;LA REGLETA NO ESTA ENCENDIDA;SOLUCION
   (regleta_encendida No)
=>
   (printout t "R// POSIBLE CAUSA, REGLETA APAGADA" crlf)
   (printout t "ACTIVE SU REGLETA E INTENTE DE NUEVO" crlf)
;   (bind ?*corregido* 1)
   (assert (CORREGIDO Si))
   (bind ?*mensaje* "LA REGLETA NO ESTABA ENCENDIDA")
   (retract 5);esto se tiene que cambiar por una variable que tome el control del fact
   (retract 2);esto se tiene que cambiar por una variable que tome el control del fact
   (retract 1);esto se ti
   (assert (arranque))
)

; ========================
;    CAUSAS Y SOLUCIONES
; ========================
(defrule CARGADOR-NO-CONECTADO "En caso el problema se por falta de carga"
   ?hecho <- (cargador-no-conectado)
   ?hecho_actual <- (EQUIPO (v_tipo Portatil) (v_enciende No) (v_bateria_funciona Si) (v_bateria_sin_carga Si))
   (cargador-conectado No)
=>
   (printout t "R// POSIBLE CAUSA, REQUIERE CARGADOR" crlf)
   (printout t "CONECTE CARGADOR E INTENTE DE NUEVO" crlf)
   (assert (CORREGIDO Si))
   (bind ?*mensaje* "EL EQUIPO REQUERIA CARGADOR")
   (retract ?hecho_actual)
   (retract ?hecho)
   (retract 1)
;   (retract 5);esto se tiene que cambiar por una variable que tome el control del fact
;   (retract 2);esto se tiene que cambiar por una variable que tome el control del fact
;   (retract 1);esto se tiene que cambiar por una variable que tome el control del fact
   (assert (arranque))
)

; ==================
;    RESULTADOS
; ==================
(defrule EQUIPO-ENCIENDE
   (EQUIPO (v_enciende Si))
   (CORREGIDO Si)
;   (CORREGIDO ?corregido&:(= 1 ?*corregido*))
=>
   (printout t ?*mensaje* crlf)
   (printout t "GRACIAS POR USAR NUESTRO SISTEMA" crlf "SALUDOS" crlf)
   (printout t "=================================================================" crlf)
)