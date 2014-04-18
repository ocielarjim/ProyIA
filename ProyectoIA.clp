; ==================
; DEFGLOBAL
; ==================
(defglobal
   ?*corregido* = 0
   ?*mensaje* = ""
   ?*localvar1* = ""
)

; ==================
; DEFTEMPLATE
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
; (allowed-symbols Extension Regleta)
   )
   (slot v_boton_ON
      (type SYMBOL)
; (allowed-symbols Si No)
   )
   (slot v_existe
      (type SYMBOL)
; (allowed-symbols Si No)
   )
)

; ==================
; INITIAL-FACT
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
; PREGUNTA INICIAL
; =====================
(defrule INICIO
   (arranque)
=>
   (printout t "DE QUE TIPO ES TU PC? (Portatil/Escritorio)" crlf)
   (bind ?tipo (read))
   (printout t "TU EQUIPO ENCIENDE? (Si/No)" crlf)
   (assert (EQUIPO (v_tipo ?tipo) (v_enciende (read))))
   (assert (inicia-validacion))
)

; ==============================
; EN CASO DE SER PC ESCRITORIO
; ==============================
(defrule EQUIPO-ESCRITORIO
   ?hecho <- (inicia-validacion)
   ?hecho_actual <- (EQUIPO (v_tipo Escritorio) (v_enciende No))
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
         (assert (cable-poder))
		 (assert (sin-extension))
   )
   (retract ?hecho)
)

; ========================
; EN CASO DE SER LAPTOP
; ========================
(defrule INICIA-VALIDACION
   ?hecho <- (inicia-validacion)
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

; ========== SOLUCION ==========
(defrule CARGADOR-NO-CONECTADO "En caso el problema se por falta de carga"
   ?hecho <- (cargador-no-conectado)
   ?hecho_actual <- (EQUIPO (v_tipo Portatil) (v_enciende No) (v_bateria_funciona Si) (v_bateria_sin_carga Si))
   ?arranque <- (arranque)
=>
   (printout t "R// POSIBLE CAUSA, REQUIERE CARGADOR" crlf)
   (printout t "CONECTE CARGADOR E INTENTE DE NUEVO" crlf)
   (assert (CORREGIDO Si))
   (bind ?*mensaje* "EL EQUIPO REQUERIA CARGADOR" crlf)
   (retract ?hecho_actual)
   (retract ?hecho)
   (retract ?arranque)
   (assert (arranque))
)

; =====================================
; PROBLEMAS CON EXTENSION O REGLETA
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
         (assert (cable-poder))
		 (assert (sin-extension))
   )
   (retract ?hecho)
)

(defrule EXTENSION-BIEN-CONECTADA
   ?hecho <- (extension-bien-conectada)
   ?hecho_extension <- (EXTENSION (v_tipo ?tipo))
=>
   (printout t "VERIFIQUE QUE LA " ?tipo " ESTE BIEN CONECTADA, ¿LO ESTA? (Si/No)" crlf)
   (bind ?conectado (read))
   (if (eq ?conectado No)
      then
         (assert (regleta_bien_conectada No))
      else
         (if (eq ?tipo Regleta)
            then
               (assert (regleta_encendida))
			else
			   (assert (cable-poder))
		 )
   )
   (retract ?hecho)
)

; ========== SOLUCION 1 ==========
(defrule EXTENSION-MAL-CONECTADA
   ?hecho <- (regleta_bien_conectada No)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
   ?hecho_extension <- (EXTENSION (v_tipo ?etipo))
   ?arranque <- (arranque)
=>
   (if (eq ?tipo Escritorio)
      then
         (bind ?tipo "PC de Escritorio")
   )
   (printout t "R// POSIBLE CAUSA, " ?etipo" MAL CONECTADA" crlf)
   (printout t "CONECTE BIEN SU " ?etipo " E INTENTE ENCENDER DE NUEVO SU " ?tipo crlf)
; (bind ?*corregido* 1)
   (assert (CORREGIDO Si))
   (bind ?*mensaje* LA ?etipo ESTABA MAL CONECTADA)
   (retract ?hecho_actual)
   (retract ?arranque)
   (retract ?hecho)
   (retract ?hecho_extension)
   (assert (arranque))
)

(defrule REGLETA-ENCENDIDA ;regla para validar que la regleta este encendida
   ?hecho_extension <- (EXTENSION (v_tipo Regleta))
   ?hecho <- (regleta_encendida)
=>
   (printout t "VERIFIQUE QUE LA REGLETA ESTE ACTIVA <ON> ¿LO ESTA? (Si/No)" crlf)
   (bind ?estaon (read))
   (if (eq ?estaon No)
   then
      (assert (regleta_encendida No))
   else
      (assert (cable-poder))
   )
)

; ========== SOLUCION 2 ==========
(defrule REGLETA-APAGADA
   ?hecho <- (regleta_encendida No)
   ?hecho_extension <- (EXTENSION (v_tipo Regleta))
   ?hecho_actual <- (EQUIPO (v_enciende No))
   ?arranque <- (arranque)
=>
   (printout t "R// POSIBLE CAUSA, REGLETA APAGADA" crlf)
   (printout t "ACTIVE SU REGLETA E INTENTE DE NUEVO" crlf)
   (assert (CORREGIDO Si))
   (bind ?*mensaje* "LA REGLETA NO ESTABA ENCENDIDA")
   (retract ?hecho)
   (retract ?hecho_extension)
   (retract ?hecho_actual)
   (retract ?arranque)
   (assert (arranque))
)

; ======================================================
; PROBLEMAS CON LA CONEXION DE CABLES A TOMA CORRIENTE
; ======================================================
(defrule CABLE-PODER
   ?hecho <- (cable-poder)
=>
   (printout t "VERIFIQUE EL CABLE QUE ALIMENTA LA FUENTE DE PODER, ESTA BIEN CONECTADO? (Si/No)" crlf)
   (bind ?cablepoder (read))
   (if (eq ?cablepoder No)
      then
         (assert (cable-poder-mal-conectado))
      else
         (assert (conectar-otro-dispositivo))
   )
   (retract ?hecho)
)

; ========== SOLUCION ==========
(defrule CABLE-PODER-MAL-CONECTADO
   ?hecho <- (cable-poder-mal-conectado)
   ?hecho_extension <- (EXTENSION (v_tipo ?tipo))
   ?hecho_actual <- (EQUIPO (v_enciende No))
   ?arranque <- (arranque)
=>
   (printout t "R// POSIBLE CAUSA, CABLE DE PODER MAL CONECTADO" crlf)
   (printout t "CORRIJA EL PROBLEMA E INTENTE DE NUEVO" crlf)
   (assert (CORREGIDO Si))
   (bind ?*mensaje* "EL CABLE DE LA FUENTE DE PODER NO ESTABA BIEN CONECTADO")
   (retract ?hecho)
   (retract ?hecho_extension)
   (retract ?hecho_actual)
   (retract ?arranque)
   (assert (arranque))
)

; ===========================================================
; FUNCIONAN OTROS DISPOSITIVOS EN LA CONEXION AAC PRINCIPAL
; ===========================================================
(defrule CONECTAR-OTRO-DISPOSITIVO-SIN-E "Sin el uso de regleta o extension"
   ?hecho1 <- (conectar-otro-dispositivo)
   ?hecho2 <- (sin-extension)
=>
   (printout t "HAS INTENTADO CONECTAR OTRO DISPOSITIVO EN EL PUNTO DONDE ESTA CONECTADA LA FUENTE DE PODER?" crlf)
   (printout t "(POR EJEMPLO: UN RADIO, UN CARGADOR DE CELULAR, ETC) (Si/No)" crlf)
   (bind ?conectar (read))
   (if (eq ?conectar No)
      then
	     (printout t "POR FAVOR, INTENTA CONECTAR OTRO DISPOSITVO, (POR EJEMPLO: PON A CARGAR TU CELULAR)" crlf)
		 (assert (conecta-otro-dispositivo))
      else
	     (assert (conecta-otro-dispositivo))
   )
   (retract ?hecho1)
   (retract ?hecho2)
)

(defrule CONECTAR-OTRO-DISPOSITIVO-CON-E "Utilizando regleta o extension"
   ?hecho <- (conectar-otro-dispositivo)
   ?hecho_extension <- (EXTENSION (v_tipo ?etipo))
=>
   (printout t "HAS INTENTADO CONECTAR OTRO DISPOSITIVO EN EL PUNTO DONDE ESTA CONECTADA LA " ?etipo crlf)
   (printout t "(POR EJEMPLO: UN RADIO, UN CARGADOR DE CELULAR, ETC) (Si/No)" crlf)
   (bind ?conectar (read))
   (if (eq ?conectar No)
      then
	     (printout t "POR FAVOR, INTENTA CONECTAR OTRO DISPOSITVO, (POR EJEMPLO: PON A CARGAR TU CELULAR)" crlf)
		 (assert (conecta-otro-dispositivo))
      else
	     (assert (conecta-otro-dispositivo))
   )
   (retract ?hecho)
)

(defrule CONECTA-OTRO-DISPOSITIVO
   ?hecho <- (conecta-otro-dispositivo)
=>
   (printout t "QUE DISPOSITIVO CONECTASTE?" crlf)
   (bind ?dispositivo (read))
   (printout t "El " ?dispositivo " FUNCIONO? (Si/No)" crlf)
   (bind ?funciona (read))
   (if (eq ?funciona Si)
      then
	     (assert (otro-dispositivo-funciona))
	  else
         (assert (punto-sin-electricidad))
   )
   (retract ?hecho)
)

(defrule PUNTO-SIN-ELECTRICIDAD "Punto de corriente sin electricidad"
   ?hecho <- (punto-sin-electricidad)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
   ?arranque <- (arranque)
=>
   (printout t "CREO QUE EL PROBLEMA ESTA EN EL PUNTO DE CORRIENTE" crlf)
   (printout t "MUEVE TODO TU EQUIPO A OTRO PUNTO DE CORRIENTE E INTENALO DE NUEVO" crlf)
   (printout t "SABEMOS QUE PUEDE SER UNA TAREA DIFICIL" crlf)
   (printout t "TOMATE TU TIEMPO..., Y ESCRIBE OK AL HABER CULMINADO ESTA ACCION" crlf)
   (read)
   (printout t "GRACIAS POR INTENTARLO" crlf)
   (printout t "OBTUBISTE EL MISMO RESULTADO AL INTENTAR MOVER TU EQUIPO A OTRO LUGAR? (Si/No)" crlf)
   (bind ?resultado (read))
   (if (eq ?resultado Si)
      then
	     (printout t "R// EL PROBLEMA PUEDE DEBERSE A PROBLEMAS EN LA RED ELECTRICA" crlf)
		 (printout t "LLAMA A ESTE NUMERO 55102892 CON GUSTO TE VISITARA UN TECNICO" crlf "CAPACITADO PARA BRINDARTE ASESORIA Y MAS INFORMACION" crlf)
		 (assert (despedida))
		 (reset)
      else
	     (printout t "APARENTEMENTE NOTASTE ALGUN CAMBIO" crlf)
		 (printout t "CAMBIA PERMANENTEMENTE TU EQUIPO DE LUGAR E INTENTA DE NUEVO" crlf)
	     (printout t "R// POSIBLE CAUSA, PUNTO DE ACC SIN ELECTRICIDAD" crlf)
         (assert (CORREGIDO Si))
         (bind ?*mensaje* "EL PUNTO DE CONEXION ACC TIENE PROBLEMAS DE CORRIENTE" crlf)
         (retract ?hecho)
         (retract ?hecho_actual)
         (retract ?arranque)
         (assert (arranque))
   )
)

; ========== SOLUCION ==========
(defrule OTRO-DISPOSITIVO-FUNCIONA
   ?hecho <- (otro-dispositivo-funciona)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
=>
   (if (eq ?tipo Portatil)
      then
	     (assert (cable-poder-averiado))
;	  else
;        PROGRAMAR EN CASO DE SER PC DE ESCRITORIO
   )
;   (reset)
;   (assert (initial-fact))
)

; =============================
; PROBLEMA CON CABLE DE PODER
; =============================
(defrule CABLE-PODER-AVERIADO
   ?hecho <- (cable-poder-averiado)
=>
   (printout t "EL SIGUIENTE PASO PUEDE SER COMPLICADO POR CARENCIA DE MATERIALES" crlf)
   (printout t "NECESITAS OTRO CABLE CON LAS MISMAS CARACTERISTICAS PARA ALIMENTAR LA FUENTE DE PODER" crlf)
   (printout t "PUEDES CONSEGUIR UN CABLE Y VERIFICAR EL COMPORTAMIENTO? (Si/No)" crlf)
   (bind ?cable_nuevo (read))
   (if (eq ?cable_nuevo Si)
      then
	     (assert (cable-poder-funciona))
	  else
	     (printout t "ENTENDEMOS TU DIFICULTAD, TRATA DE CONSEGUIR UN CABLE SIMILAR E INTENTA MAS TARDE" crlf)
		 (printout t "LLAMA A ESTE NUMERO 55102892 CON GUSTO TE VISITARA UN TECNICO" crlf "CAPACITADO PARA BRINDARTE ASESORIA Y MAS INFORMACION" crlf)
		 (assert (despedida))
		 (reset)
   )
   (retract ?hecho)
)

(defrule CABLE-PODER-FUNCIONA
   ?hecho <- (cable-poder-funciona)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
   ?arranque <- (arranque)
=>
   (printout t "REMPLAZA EL CABLE DE PODER E INTENTA VER QUE SUCEDE" crlf)
   (printout t "TOMATE TU TIEMPO..., Y ESCRIBE OK AL HABER CULMINADO ESTA ACCION" crlf)
   (read)
   (printout t "GRACIAS POR INTENTARLO" crlf)
   (printout t "OBTUBISTE EL MISMO RESULTADO AL INTENTAR CON OTRO CABLE DE PODER? (Si/No)" crlf)
   (bind ?resultado (read))
   (if (eq ?resultado Si)
      then
	     (printout t "R// TIENES PROBLEMAS CON EL CARGADOR DE TU " ?tipo ", TE RECOMENDAMOS COMPRAR UNO NUEVO" crlf)
		 (printout t "LLAMA A ESTE NUMERO 55102892 CON GUSTO TE VISITARA UN TECNICO" crlf "CAPACITADO PARA BRINDARTE ASESORIA Y MAS INFORMACION" crlf)
		 (assert (despedida))
		 (reset)
      else
	     (printout t "APARENTEMENTE NOTASTE ALGUN CAMBIO" crlf)
		 (printout t "CAMBIA PERMANENTEMENTE EL CABLE E INTENTA DE NUEVO" crlf)
	     (printout t "R// POSIBLE CAUSA, CABLE DE PODER AVERIADO" crlf)
         (assert (CORREGIDO Si))
         (bind ?*mensaje* "EL CABLE DE LA FUENTE ESTABA AVERIADO")
         (retract ?hecho)
         (retract ?hecho_actual)
         (retract ?arranque)
         (assert (arranque))
   )
)

; ==================
; RESULTADOS
; ==================
(defrule EQUIPO-ENCIENDE
   (EQUIPO (v_enciende Si))
   (CORREGIDO Si)
; (CORREGIDO ?corregido&:(= 1 ?*corregido*))
=>
   (printout t ?*mensaje* crlf)
   (assert (despedida))
; (reset)
)

(defrule DESPEDIDA
   (despedida)
=>
   (printout t "GRACIAS POR USAR NUESTRO SISTEMA" crlf "SALUDOS" crlf)
   (printout t "=================================================================" crlf)
)
