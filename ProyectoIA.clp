; ==================
; DEFGLOBAL
; ==================
(defglobal
   ?*corregido* = 0
   ?*mensaje* = ""
   ?*localvar1* = ""
   ?*escribe_respuesta* = "ESCRIBE EL NUMERO CORRESPONDIENTE A TU RESPUESTA Y PRESIONA ENTER:"
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
   (slot v_pantalla_enciende
      (type SYMBOL)
   )
   (slot v_bateria_sin_carga
      (type SYMBOL)
   )
   (slot v_disco_duro_ruidoso
      (type SYMBOL)
   )
   (slot v_bios_demora
      (type SYMBOL)
   )
   (slot v_scandisk
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

(deftemplate ARRANCA
   (slot v_arranca
      (type SYMBOL)
   )
)

(deftemplate ENCIENDE
   (slot v_enciende
      (type SYMBOL)
   )
)

; ==============
; DEFFUNCTION
; ===============
(deffunction RESP_SI_NO
   (?valor)
   (printout t "   1. Si" crlf "   2. No" crlf ?*escribe_respuesta*)
)

(deffunction ASIGNA_RESP
   (?valor)
   (if (or(eq ?valor 1) (eq ?valor 2))
      then
         (if (eq ?valor 1)
            then
               Si
	        else
               No
		 )
      else
	     (printout t "RESPUESTA INVALIDA, INGRESE UNA RESPUESTA VALIDA Y PRESIONA ENTER:")
		 (ASIGNA_RESP (read))
   )
)

(deffunction PUBLICIDAD
   (?valor)
   (printout t "LLAMA A ESTE NUMERO 55102892 CON GUSTO TE VISITARA UN TECNICO" crlf "CAPACITADO PARA BRINDARTE ASESORIA Y MAS INFORMACION" crlf)
)

(deffunction DESPEDIDA
   (?valor)
=>
   (printout t "GRACIAS POR USAR NUESTRO SISTEMA" crlf "SALUDOS" crlf)
   (printout t "=================================================================" crlf)
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
   (printout t "" crlf)
   (printout t "GRACIAS, AHORA AYUDAME CONTESTANDO ALGUNAS PREGUNTAS:" crlf)
   (assert (arranque))
   (assert (ARRANCA (v_arranca No)))
   (assert (ENCIENDE (v_enciende No)))
)

; =====================
; PREGUNTA INICIAL
; =====================
(defrule INICIO
   (arranque)
=>
   (printout t "DE QUE TIPO ES TU PC?" crlf)
   (printout t "   1. Portatil" crlf)
   (printout t "   2. PC Escritorio" crlf)
   (printout t ?*escribe_respuesta*)
   (bind ?tipo (read))
   (if (eq ?tipo 1)
      then
	     (bind ?tipo Portatil)
	  else
	     (bind ?tipo PC Escritorio)
   )
   (printout t "" crlf)
   (printout t "TU EQUIPO ENCIENDE?" crlf)
   (RESP_SI_NO nil)
   (assert (EQUIPO (v_tipo ?tipo) (v_enciende (ASIGNA_RESP (read)))))
   (printout t "" crlf)
   (assert (inicia-validacion))
)

; ==================================================
; EQUIPO NO ENCIENDE EN CASO DE SER PC ESCRITORIO 
; ==================================================
(defrule EQUIPO-ESCRITORIO
   ?hecho <- (inicia-validacion)
   ?hecho_actual <- (EQUIPO (v_tipo Escritorio) (v_enciende No))
=>
   (printout t "UTILIZA ALGUNA EXTENSION O REGLETA?" crlf)
   (RESP_SI_NO nil)
   (bind ?existe (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?existe Si)
      then
         (printout t "QUE TIPO DE EXTENSION USA?" crlf)
         (printout t "1. Extension" crlf)
         (printout t "2. Regleta" crlf)
         (printout t ?*escribe_respuesta* crlf)
         (bind ?tipo (read))
		 (printout t "" crlf)
         (if (eq ?tipo 1)
		    then
			   ?tipo = Extension
			else
			   ?tipo = Regleta
		 )
         (assert (EXTENSION (v_existe ?existe) (v_tipo ?tipo)))
         (assert (extension-bien-conectada))
      else
         (assert (cable-poder))
		 (assert (sin-extension))
   )
   (retract ?hecho)
)

; ===========================================
; EQUIPO NO ENCIENDE EN CASO DE SER LAPTOP
; ===========================================
(defrule INICIA-VALIDACION
   ?hecho <- (inicia-validacion)
   ?hecho_actual <- (EQUIPO (v_tipo Portatil) (v_enciende No))
=>
   (printout t "AUN FUNCIONA LA BATERIA DE SU PORTATIL?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
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
   (printout t "EL INDICADOR DE BATERIA EN SU PORTATIL DA LA SENAL DE FALTA DE CARGA?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (if (eq ?valor Si)
      then
         (modify ?hecho_actual (v_bateria_sin_carga Si))
         (assert (indicador-bajo))
      else
         (modify ?hecho_actual (v_bateria_sin_carga No))
         (assert (valida-extension))
   )
   (retract ?hecho)
   (printout t "" crlf)
)

(defrule INDICADOR-BAJO
   ?hecho <- (indicador-bajo)
   ?hecho_actual <- (EQUIPO (v_tipo Portatil) (v_enciende No) (v_bateria_funciona Si) (v_bateria_sin_carga Si))
=>
   (printout t "TIENE CONECTADO EL CARGADOR EN SU PORTATIL?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (if (eq ?valor No)
      then
         (assert (cargador-conectado No))
         (assert (cargador-no-conectado))
      else
         (assert (cargador-conectado Si))
         (assert (valida-extension))
   )
   (retract ?hecho)
   (printout t "" crlf)
)

; ========== SOLUCION ==========
(defrule CARGADOR-NO-CONECTADO "En caso el problema se por falta de carga"
   ?hecho <- (cargador-no-conectado)
   ?hecho_actual <- (EQUIPO (v_tipo Portatil) (v_enciende No) (v_bateria_funciona Si) (v_bateria_sin_carga Si))
   ?arranque <- (arranque)
   ?enciende <- (ENCIENDE (v_enciende ?v_enc))
=>
   (printout t "R// POSIBLE CAUSA, REQUIERE CARGADOR" crlf)
   (printout t "CONECTE CARGADOR E INTENTE DE NUEVO" crlf)
   (modify  ?enciende (v_enciende Si))
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
   (printout t "UTILIZA ALGUNA EXTENSION O REGLETA" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
         (printout t "QUE TIPO DE EXTENSION USA?" crlf)
         (printout t "   1. Extension" crlf)
         (printout t "   2. Regleta" crlf)
         (printout t ?*escribe_respuesta*)
         (bind ?tipo (read))
		 (printout t "" crlf)
         (if (eq ?tipo 1)
		    then
			   (bind ?tipo Extension)
			else
			   (bind ?tipo Regleta)
		 )
         (assert (EXTENSION (v_existe ?valor) (v_tipo ?tipo)))
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
   (printout t "VERIFIQUE QUE LA " ?tipo " ESTE BIEN CONECTADA, ¿LO ESTA?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (if (eq ?valor No)
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
   (printout t "" crlf)
)

; ========== SOLUCION 1 ==========
(defrule EXTENSION-MAL-CONECTADA
   ?hecho <- (regleta_bien_conectada No)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
   ?hecho_extension <- (EXTENSION (v_tipo ?etipo))
   ?arranque <- (arranque)
   ?enciende <- (ENCIENDE (v_enciende ?venc))
=>
   (if (eq ?tipo Escritorio)
      then
         (bind ?tipo "PC de Escritorio")
   )
   (printout t "R// POSIBLE CAUSA, " ?etipo" MAL CONECTADA" crlf)
   (printout t "CONECTE BIEN SU " ?etipo " E INTENTE ENCENDER DE NUEVO SU " ?tipo crlf)
; (bind ?*corregido* 1)
   (modify  ?enciende (v_enciende Si))
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
   (printout t "VERIFIQUE QUE LA REGLETA ESTE ACTIVA <ON> ¿LO ESTA?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (if (eq ?valor No)
   then
      (assert (regleta_encendida No))
   else
      (assert (cable-poder))
   )
   (printout t "" crlf)
   (retract ?hecho)
)

; ========== SOLUCION 2 ==========
(defrule REGLETA-APAGADA
   ?hecho <- (regleta_encendida No)
   ?hecho_extension <- (EXTENSION (v_tipo Regleta))
   ?hecho_actual <- (EQUIPO (v_enciende No))
   ?arranque <- (arranque)
   ?enciende <- (ENCIENDE (v_enciende ?venc))
=>
   (printout t "R// POSIBLE CAUSA, REGLETA APAGADA" crlf)
   (printout t "ACTIVE SU REGLETA E INTENTE DE NUEVO" crlf)
   (modify  ?enciende (v_enciende Si))
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
   (printout t "VERIFIQUE EL CABLE QUE ALIMENTA LA FUENTE DE PODER, ESTA BIEN CONECTADO?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (if (eq ?valor No)
      then
         (assert (cable-poder-mal-conectado))
      else
         (assert (conectar-otro-dispositivo))
   )
   (retract ?hecho)
   (printout t "" crlf)
)

; ========== SOLUCION ==========
(defrule CABLE-PODER-MAL-CONECTADO
   ?hecho <- (cable-poder-mal-conectado)
   ?hecho_extension <- (EXTENSION (v_tipo ?tipo))
   ?hecho_actual <- (EQUIPO (v_enciende No))
   ?arranque <- (arranque)
   ?enciende <- (ENCIENDE (v_enciende ?venc))
=>
   (printout t "R// POSIBLE CAUSA, CABLE DE PODER MAL CONECTADO" crlf)
   (printout t "CORRIJA EL PROBLEMA E INTENTE DE NUEVO" crlf)
   (modify  ?enciende (v_enciende Si))
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
   (printout t "(POR EJEMPLO: UN RADIO, UN CARGADOR DE CELULAR, ETC)" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (if (eq ?valor No)
      then
	     (printout t "POR FAVOR, INTENTA CONECTAR OTRO DISPOSITVO, (POR EJEMPLO: PON A CARGAR TU CELULAR)" crlf)
		 (assert (conecta-otro-dispositivo))
      else
	     (assert (conecta-otro-dispositivo))
   )
   (retract ?hecho1)
   (retract ?hecho2)
   (printout t "" crlf)
)

(defrule CONECTAR-OTRO-DISPOSITIVO-CON-E "Utilizando regleta o extension"
   ?hecho <- (conectar-otro-dispositivo)
   ?hecho_extension <- (EXTENSION (v_tipo ?etipo))
=>
   (printout t "HAS INTENTADO CONECTAR OTRO DISPOSITIVO EN EL PUNTO DONDE ESTA CONECTADA LA " ?etipo crlf)
   (printout t "(POR EJEMPLO: UN RADIO, UN CARGADOR DE CELULAR, ETC)" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (if (eq ?valor No)
      then
	     (printout t "POR FAVOR, INTENTA CONECTAR OTRO DISPOSITVO, (POR EJEMPLO: PON A CARGAR TU CELULAR)" crlf)
		 (assert (conecta-otro-dispositivo))
      else
	     (assert (conecta-otro-dispositivo))
   )
   (retract ?hecho)
   (printout t "" crlf)
)

(defrule CONECTA-OTRO-DISPOSITIVO
   ?hecho <- (conecta-otro-dispositivo)
=>
   (printout t "QUE DISPOSITIVO CONECTASTE?" crlf)
   (bind ?dispositivo (read))
   (printout t "El " ?dispositivo " FUNCIONO? (Si/No)" crlf)
   (bind ?funciona (read))
   (printout t "" crlf)
   (if (eq ?funciona Si)
      then
	     (assert (otro-dispositivo-funciona))
	  else
         (assert (punto-sin-electricidad))
   )
   (retract ?hecho)
   (printout t "" crlf)
)

(defrule PUNTO-SIN-ELECTRICIDAD "Punto de corriente sin electricidad"
   ?hecho <- (punto-sin-electricidad)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
   ?arranque <- (arranque)
   ?enciende <- (ENCIENDE (v_enciende ?venc))
=>
   (printout t "CREO QUE EL PROBLEMA ESTA EN EL PUNTO DE CORRIENTE" crlf)
   (printout t "MUEVE TODO TU EQUIPO A OTRO PUNTO DE CORRIENTE E INTENALO DE NUEVO" crlf)
   (printout t "SABEMOS QUE PUEDE SER UNA TAREA DIFICIL" crlf)
   (printout t "TOMATE TU TIEMPO..., Y ESCRIBE OK AL HABER CULMINADO ESTA ACCION" crlf)
   (read)
   (printout t "" crlf)
   (printout t "GRACIAS POR INTENTARLO" crlf)
   (printout t "OBTUBISTE EL MISMO RESULTADO AL INTENTAR MOVER TU EQUIPO A OTRO LUGAR?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (printout t "R// EL PROBLEMA PUEDE DEBERSE A PROBLEMAS EN LA RED ELECTRICA" crlf)
		 (PUBLICIDAD nil)
		 (DESPEDIDA nil)
		 (reset)
      else
	     (printout t "APARENTEMENTE NOTASTE ALGUN CAMBIO" crlf)
		 (printout t "CAMBIA PERMANENTEMENTE TU EQUIPO DE LUGAR E INTENTA DE NUEVO" crlf)
	     (printout t "R// POSIBLE CAUSA, PUNTO DE ACC SIN ELECTRICIDAD" crlf)
         (modify  ?enciende (v_enciende Si))
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
   (printout t "PUEDES CONSEGUIR UN CABLE Y VERIFICAR EL COMPORTAMIENTO?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (assert (cable-poder-funciona))
	  else
	     (printout t "ENTENDEMOS TU DIFICULTAD, TRATA DE CONSEGUIR UN CABLE SIMILAR E INTENTA MAS TARDE" crlf)
		 (PUBLICIDAD nil)
		 (DESPEDIDA nil)
		 (reset)
   )
   (retract ?hecho)
)

(defrule CABLE-PODER-FUNCIONA
   ?hecho <- (cable-poder-funciona)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
   ?arranque <- (arranque)
   ?enciende <- (ENCIENDE (v_enciende ?venc))
=>
   (printout t "REMPLAZA EL CABLE DE PODER E INTENTA VER QUE SUCEDE" crlf)
   (printout t "TOMATE TU TIEMPO..., Y ESCRIBE OK AL HABER CULMINADO ESTA ACCION" crlf)
   (read)
   (printout t "GRACIAS POR INTENTARLO" crlf)
   (printout t "OBTUBISTE EL MISMO RESULTADO AL INTENTAR CON OTRO CABLE DE PODER?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (printout t "R// TIENES PROBLEMAS CON EL CARGADOR DE TU " ?tipo ", TE RECOMENDAMOS COMPRAR UNO NUEVO" crlf)
		 (PUBLICIDAD nil)
		 (DESPEDIDA nil)
		 (reset)
      else
	     (printout t "APARENTEMENTE NOTASTE ALGUN CAMBIO" crlf)
		 (printout t "CAMBIA PERMANENTEMENTE EL CABLE E INTENTA DE NUEVO" crlf)
	     (printout t "R// POSIBLE CAUSA, CABLE DE PODER AVERIADO" crlf)
         (modify  ?enciende (v_enciende Si))
         (bind ?*mensaje* "EL CABLE DE LA FUENTE ESTABA AVERIADO")
         (retract ?hecho)
         (retract ?hecho_actual)
         (retract ?arranque)
         (assert (arranque))
   )
)

; =================
; EQUIPO ENCIENDE
; =================
(defrule EQUIPO-ENCIENDE-L
   ?hecho <- (inicia-validacion)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo) (v_enciende Si))
   (ENCIENDE (v_enciende No))
=>
   (printout t "LA PANTALLA ENCIENDE MOSTRANDO LOS SIMBOLOS E IMAGENES HABITUALES" crlf)
   (printout t "QUE SE ACTIVAN AL RECIEN ENCENDER EL EQUIPO (BIOS)?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (modify ?hecho_actual (v_pantalla_enciende Si))
	  else
	     (modify ?hecho_actual (v_pantalla_enciende No))
   )
   (assert (problema-bios))
   (retract ?hecho)
)

; =======================
; PROBLEMA CON LA BIOS
; =======================
(defrule BIOS-INIT
   ?hecho <- (problema-bios)
   ?hecho_actual <- (EQUIPO (v_pantalla_enciende Si))
=>
   (printout t "ESA MISMA PANTALLA DEMORA MAS DE 10 MIN EN PASAR AL SPLASH DE ARRANQUE DE WINDOWS" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (modify ?hecho_actual (v_bios_demora Si))
	     (assert (lentitud-bios-no-solventado))
	     (assert (dispositivos-masivos))
	  else
	     (modify ?hecho_actual (v_bios_demora No))
	     (assert (verifica-disco))
		 (assert (lentitud-bios-solventado))
   )
   (retract ?hecho)
)

(defrule DISPOSITIVOS-MASIVOS
   ?hecho <- (dispositivos-masivos)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
   ?arranque <- (arranque)
   ?hecho_arranca <- (ARRANCA (v_arranca No))
=>
   (printout t "VERIFIQUE QUE EN SU EQUIPO NO HAYAN CONECTADOS DISPOSITIVOS PORTATILES" crlf)
   (printout t "DE ALMACENAMIENTO (POR EJEMPLO MEMORIAS USB, CD O FLOPPY, ¿LO HAY?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (printout t "RETIRE LOS DISPOSITIVOS E INTENTE DE NUEVO" crlf)
		 (printout t "R// POSIBLE CAUSA, AMBIGUEDAD EN EL ARRANQUE DE LA BIOS" crlf)
         (modify ?hecho_arranca (v_arranca Si))
         (bind ?*mensaje* "UN DISPOSITIVO EXTERNO PRODUJO UN MAL ARRANQUE EN TU EQUIPO" crlf)
         (retract ?hecho_actual)
         (retract ?arranque)
         (assert (arranque))
	  else
	     (assert (bios-demora))
		 (assert (verifica-disco))
   )
   (retract ?hecho)
)

(defrule DISPOSITIVOS-MASIVOS-RETIRADOS
   ?hecho1 <- (lentitud-bios-no-solventado)
   ?hecho2 <- (ARRANCA (v_arranca Si))
=>
   (printout t "APARENTEMENTE LOS DISPOSITIVOS NO ESTABAN OCASIONANDO PROBLEMAS, SIN EMBARGO," crlf)
   (printout t "TE RECOMENDAMOS MANTENERLOS DESCONECTADOS MIENTRAS CONTINUAMOS CON EL DIAGNOSTICO" crlf)
   (printout t "DE TU EQUIPO" crlf)
   (assert (verifica-disco))
   (retract ?hecho1)
   (retract ?hecho2)
)

(defrule BIOS-DEMORA
   ?hecho1 <- (bios-demora)
   ?hecho2 <- (ARRANCA (v_arranca Si))
=>
   (printout t "TOMA UN LAPIZ Y ANOTA TODO LO QUE ALCANCES A VER EN ESTA PANTALLA" crlf)
   (printout t "TE SERA DE UTILIDAD EN CASO TU EQUIPO CONTINUA SIN PODER CARGAR WINDOWS" crlf)
   (retract ?hecho1)
   (retract ?hecho2)
   (assert (verifica-disco))
)

; ========== SOLUCION ==========
(defrule LENTITUD-BIOS-SOLVENTADO
   ?hecho <- (lentitud-bios-solventado)
   (ARRANCA (v_arranca Si))
=>
   (printout t ?*mensaje* crlf)
   (DESPEDIDA nil)
;   (reset)
)

; =================================================
; PROBLEMAS CON DISCO DURO O ARRANQUE DE WINDOWS
; =================================================
(defrule VERIFICA-DISCO
   ?hecho <- (verifica-disco)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
=>
   (printout t "ESCUCHAS RUIDO DENTRO DE TU EQUIPO (UN RUIDO PARECIDO AL CHOQUE DE 2 METALES FINOS)" crlf)
   (RESP_SI_NO nil)
   (modify ?hecho_actual (v_disco_duro_ruidoso (ASIGNA_RESP (read))))
   (printout t "" crlf)
   (retract ?hecho)
   (assert (activa-scandisk))
)

(defrule ACTIVA-SCANDISK
   ?hecho <- (activa-scandisk)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
=>
   (printout t "EN OCASIONES LE PIDE REALIZAR UN ESCANEO DE SU DISCO DURO," crlf)
   (printout t "O VE REFLEJADA LA PALABRA SCANDISK" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
         (assert (lentitud-equipo))
	  else
	     (assert (modo-arranque))
   )
   (modify ?hecho_actual (v_scandisk ?valor))
   (retract ?hecho)
)

(defrule MODO-ARRANQUE
   ?hecho <- (modo-arranque)
=>
   (printout t "EN OCASIONES (CASI SIEMPRE) LE PIDE ELEGIR EL MODO CON EL QUE QUIERE" crlf)
   (printout t "QUE ARRANQUE WINDOWS (POR EJEMPLO: SAFE MODE O MODO SEGURO)?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (printout t "R// TIENES UNO DE 2 POSIBLES PROBLEMAS:" crlf)
		 (printout t "   1. POSIBLES PROBLEMAS CON TU DISCO DURO, POCO PROBABLE DADO QUE NO ESCUCHASTE RUIDO" crlf)
		 (printout t "   2. ES PROBABLE QUE UN VIRUS ESTE AFECTANDO EL ARRANQUE DE TU SISTEMA OPERATIVO" crlf)
		 (PUBLICIDAD nil)
		 (DESPEDIDA nil)
		 (reset)
	  else
	     (assert (windows-carga))
   )
   (retract ?hecho)
)

(defrule WINDOWS-CARGA
   ?hecho <- (windows-carga)
=>
   (printout t "EL SISTEMA OPERATIVO LOGRA LLEGAR AL LOGIN O A LEVANTAR EL ESCRITORIO?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor No)
      then
	     (assert (windows-carga-congelado))
	  else
	     (assert (windows-trabaja-lento))
   )
   (retract ?hecho)
)

(defrule WINDOWS-CARGA-CONGELADO
   ?hecho <- (windows-carga-congelado)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
=>
   (printout t "EL EQUIPO SE CONGELA DURANTE LA CARGA DE WINDOWS?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (assert (windows-congelado))
	  else
	     (printout t "SI NO LOGRAS VER EL LOGIN O EL ESCRITORIO DE WINDOWS, ESO ME DA A ENTENDER" crlf)
         (printout t "QUE NI SIQUIERA LOGRAS LLEGAR AL SPLASH DE WINDOWS" crlf)
	     (assert (windows-no-congelado))
   )
   (retract ?hecho)
)
;v_disco_duro_ruidoso, v_bios_demora, v_scandisk
; ========== SOLUCION 1 ==========
(defrule WINDOWS-NO-CONGELADO-FIN1
   (windows-no-congelado)
   (EQUIPO (v_disco_duro_ruidoso Si) (v_bios_demora Si) (v_scandisk Si))
=>
   (printout t "R// TIENES GRAVES PROBLEMAS CON EL DISCO DURO," crlf)
   (printout t "TE RECOMENDAMOS CONSULTAR CON ALGUIEN PARA INTENTAR RECUPERAR LA INFORMACION Y" crlf)
   (printout t "DE PREFERENCIA CAMBIARLO DE FORMA INMEDIATA" crlf)
   (PUBLICIDAD nil)
   (DESPEDIDA nil)
   (reset)
)

; ========== SOLUCION 2 ==========
(defrule WINDOWS-NO-CONGELADO-FIN2
   (windows-no-congelado)
   (EQUIPO (v_disco_duro_ruidoso Si) (v_bios_demora No) (v_scandisk Si))
=>
   (printout t "R// TIENES PROBLEMAS CON EL DISCO DURO, APARENTEMENTE ALGUNOS SECTORES ESTAN MALOS" crlf)
   (printout t "TE RECOMENDAMOS CONSULTAR CON ALGUIEN PARA INTENTAR RECUPERAR LA INFORMACION Y" crlf)
   (printout t "DE PREFERENCIA CAMBIARLO DE FORMA INMEDIATA" crlf)
   (PUBLICIDAD nil)
   (DESPEDIDA nil)
   (reset)
)

; ========== SOLUCION 2 ==========
(defrule WINDOWS-NO-CONGELADO-FIN3
   (windows-no-congelado)
   (EQUIPO (v_disco_duro_ruidoso Si) (v_bios_demora Si) (v_scandisk No))
=>
   (printout t "R// TIENES PROBLEMAS GRAVES CON EL DISCO DURO" crlf)
   (printout t "TE RECOMENDAMOS CONSULTAR CON ALGUIEN PARA INTENTAR RECUPERAR LA INFORMACION Y" crlf)
   (printout t "CAMBIARLO PARA SOLVENTAR TU PROBLEMA" crlf)
   (PUBLICIDAD nil)
   (DESPEDIDA nil)
   (reset)
)

; ========== SOLUCION 3 ==========
(defrule WINDOWS-NO-CONGELADO-FIN4
   (windows-no-congelado)
   (EQUIPO (v_disco_duro_ruidoso No) (v_bios_demora ?valor) (v_scandisk Si))
=>
   (printout t "R// TIENES PROBLEMAS CON EL DISCO DURO, APARENTEMENTE LOS SECTORES MALOS" crlf)
   (printout t "AUNQUE NO MUESTRA SINTOMAS GRAVES DE ESTAR MAL, ES POSIBLE QUE CIERTOS" crlf)
   (printout t "SECTORES NO MARCHEN CORRECTAMENTE, SUGERIMOS LA SUPERVISION DE UN TECNICO" crlf)
   (PUBLICIDAD nil)
   (DESPEDIDA nil)
   (reset)
)

; ========== SOLUCION 4 ==========
(defrule WINDOWS-NO-CONGELADO-FIN5
   (windows-no-congelado)
   (EQUIPO (v_disco_duro_ruidoso No) (v_bios_demora No) (v_scandisk No))
=>
   (printout t "R// POSIBLES PROBLEMAS CON EL PROCESADOR DE EQUIPO" crlf)
   (printout t "EN ESOS CASOS LO MEJOR ES COMUNICARTE CON UN TECNICO PARA QUE VALIDE" crlf)
   (PUBLICIDAD nil)
   (DESPEDIDA nil)
   (reset)
)

(defrule WINDOWS-CONGELADO-FIN1
   (windows-congelado)
   (EQUIPO (v_bios_demora No))
=>
   (printout t "MUY DE VEZ EN CUANDO LOGRAS ENTRAR A WINDOWS, PERO MIENTRAS TRABAJAS EL EQUIPO TAMBIEN SE CONGELA?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (printout t "R// TIENES UNO DE 2 POSIBLES PROBLEMAS:" crlf)
	  else
	     (printout t "ESO ME DA A ENTENDER QUE DESDE QUE TU EQUIPO ENFERMO" crlf)
		 (printout t "NO HAS PODIDO CARGAR WINDOWS CORRECTAMENTE," crlf)
	     (printout t "R// DE SER ASI, TIENES UNO DE 2 POSIBLES PROBLEMAS:" crlf)
   )
   (printout t "   1. POSIBLES PROBLEMAS CON EL PROCESADOR, MUY PROBABLE" crlf)
   (printout t "   2. POSIBLES PROBLEMAS CON EL DISCO DURO, POCO PROBABLE" crlf)
   (PUBLICIDAD nil)
   (DESPEDIDA nil)
   (reset)
)

(defrule WINDOWS-CONGELADO-FIN2
   (windows-congelado)
   (EQUIPO (v_bios_demora Si))
=>
   (printout t "MUY DE VEZ EN CUANDO LOGRAS ENTRAR A WINDOWS, PERO MIENTRAS TRABAJAS EL EQUIPO TAMBIEN SE CONGELA?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (printout t "R// TIENES UNO DE 2 POSIBLES PROBLEMAS:" crlf)
	  else
	     (printout t "ESO ME DA A ENTENDER QUE DESDE QUE TU EQUIPO ENFERMO" crlf)
		 (printout t "NO HAS PODIDO CARGAR WINDOWS CORRECTAMENTE," crlf)
	     (printout t "R// DE SER ASI, TIENES UNO DE 2 POSIBLES PROBLEMAS:" crlf)
   )
   (printout t "   1. POSIBLES PROBLEMAS CON EL DISCO DURO, MUY PROBABLE" crlf)
   (printout t "   2. POSIBLES PROBLEMAS CON EL PROCESADOR, POCO PROBABLE" crlf)
   (PUBLICIDAD nil)
   (DESPEDIDA nil)
   (reset)
)

(defrule WINDOWS-FUNCIONA-LENTO
   ?hecho <- (windows-lento)
=>
   (printout t "LOGRAS TRABAJAR, PERO NOTAS QUE EL EQUIPO TRABAJA MUY LENTO?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (assert (equipo-congelado))
	  else
         (printout t "R// VUELVE A REINICIAR TU EQUIPO, ESTA VEZ ANOTA DETENIDAMENTE LOS MENSAJES QUE BIOS" crlf)
		 (printout t "TE DESPLIGUE DURANTE LA ESPERA, CONSERVA LA INFORMACION ES PROBABLE QUE TENGAS PROBLEMA" crlf)
		 (printout t "CON ALGUN DISPOSITIVO INTERNO O QUE DEBAS RECONFIGURAR LA BIOS" crlf)
		 (PUBLICIDAD nil)
		 (DESPEDIDA nil)
		 (reset)
   )
)

(defrule EQUIPO-CONGELADO-FIN1
   ?hecho <- (equipo-congelado)
   (EQUIPO (v_bios_demora Si))
=>
   (printout t "EN OCASIONES SU EQUIPO SE CONGELA Y TE OBLIGA A REINICIAR?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
         (printout t "R// TIENES UNO DE 2 POSIBLES PROBLEMAS:" crlf)
		 (printout t "   1. POSIBLES PROBLEMAS CON TU DISCO DURO, MUY PROBABLE" crlf)
		 (printout t "   2. POSIBLES PROBLEMAS CON EL PROCESADOR, POCO PROBABLE" crlf)
	  else
	     (printout t "R// TIENES PROBLEMAS CON TU DISCO DURO:" crlf)
		 (printout t "TE RECOMENDAMOS COMENZAR A GUARDAR TU INFORMACION" crlf)
   )
   (PUBLICIDAD nil)
   (DESPEDIDA nil)
   (reset)
)

(defrule EQUIPO-CONGELADO-FIN2
   ?hecho <- (equipo-congelado)
   (EQUIPO (v_bios_demora No))
=>
   (printout t "EN OCASIONES SU EQUIPO SE CONGELA Y TE OBLIGA A REINICIAR?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
         (printout t "R// TIENES UNO DE 2 POSIBLES PROBLEMAS:" crlf)
		 (printout t "   1. POSIBLES PROBLEMAS CON EL PROCESADOR, MUY PROBABLE" crlf)
		 (printout t "   2. POSIBLES PROBLEMAS CON TU DISCO DURO, POCO PROBABLE" crlf)
	  else
	     (printout t "R// TIENES PROBLEMAS LEVES CON TU DISCO DURO:" crlf)
		 (printout t "TE RECOMENDAMOS COMENZAR A GUARDAR TU INFORMACION" crlf)
   )
   (PUBLICIDAD nil)
   (DESPEDIDA nil)
   (reset)
)

; ========================
; SITUACIONES GENERICAS
; ========================
(defrule LENTITUD-EQUIPO-FIN1
   ?hecho <- (lentitud-equipo)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo) (v_disco_duro_ruidoso Si))
=>
   (if (eq ?tipo Escritorio)
      then
         (bind ?tipo "PC de Escritorio")
   )
   (printout t "EN OCASIONES ANTERIORES, NOTO LENTITUD AL USAR EL EQUIPO" ?tipo "," crlf)
   (printout t "Y ESA LENTITUD VENINA ACOMPANADA DE REPENTINAS PANTALLAS AZULES" crlf)
   (printout t "QUE OBLIGABAN A REINICIAR EL EQUIPO?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (printout t "R// LAMENTO INFORMARTE QUE EL DISCO DURO DE TU " ?tipo ", ESTA FALLANDO Y DEBES CAMBIARLO" crlf)
	  else
	     (printout t "R// TIENES UNO DE 2 POSIBLES PROBLEMAS:" crlf)
		 (printout t "   1. POSIBLES PROBLEMAS CON TU DISCO DURO, SECTORES DE ARRANQUE DANADOS, MUY PROBABLE" crlf)
		 (printout t "   2. ES PROBABLE QUE UN VIRUS ESTE AFECTANDO EL ARRANQUE DE TU SISTEMA OPERATIVO, POCO PROBABLE" crlf)
   )
   (PUBLICIDAD nil)
   (DESPEDIDA nil)
   (reset)
)

(defrule LENTITUD-EQUIPO-FIN2
   ?hecho <- (lentitud-equipo)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo) (v_disco_duro_ruidoso No))
=>
   (if (eq ?tipo Escritorio)
      then
         (bind ?tipo "PC de Escritorio")
   )
   (printout t "EN OCASIONES ANTERIORES, NOTO LENTITUD AL USAR EL EQUIPO" ?tipo "," crlf)
   (printout t "Y ESA LENTITUD VENINA ACOMPANADA DE REPENTINAS PANTALLAS AZULES" crlf)
   (printout t "QUE OBLIGABAN A REINICIAR EL EQUIPO?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (printout t "R// LAMENTO INFORMARTE QUE EL DISCO DURO DE TU " ?tipo ", ESTA FALLANDO Y DEBES CAMBIARLO" crlf)
	  else
	     (printout t "R// TIENES UNO DE 2 POSIBLES PROBLEMAS:" crlf)
		 (printout t "   1. ES PROBABLE QUE UN VIRUS ESTE AFECTANDO EL ARRANQUE DE TU SISTEMA OPERATIVO, MUY PROBALE" crlf)
		 (printout t "   2. POSIBLES PROBLEMAS CON TU DISCO DURO, POCO PROBABLE DADO QUE NO ESCUCHASTE RUIDO" crlf)
   )
   (PUBLICIDAD nil)
   (DESPEDIDA nil)
   (reset)
)

(defrule LENTITUD-EQUIPO-FIN3
   ?hecho1 <- (lentitud-equipo)
   ?hecho2 <- (problema-procesador)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
=>
   (if (eq ?tipo Escritorio)
      then
         (bind ?tipo "PC de Escritorio")
   )
   (printout t "EN OCASIONES ANTERIORES, NOTO LENTITUD AL USAR EL EQUIPO" ?tipo "," crlf)
   (printout t "Y ESA LENTITUD VENINA ACOMPANADA DE REPENTINAS PANTALLAS AZULES" crlf)
   (printout t "QUE OBLIGABAN A REINICIAR EL EQUIPO?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (printout t "R// EL PROCESADOR DE TU " ?tipo ", ESTA FALLANDO Y DEBES CAMBIARLO" crlf)
	  else
	     (printout t "R// TIENES UNO DE 2 POSIBLES PROBLEMAS:" crlf)
		 (printout t "   1. POSIBLES PROBLEMAS CON TU PROCESADOR, MUY PROBABLE" crlf)
		 (printout t "   2. POSIBLES PROBLEMAS CON TU MOTHERBOARD, POCO PROBABLE" crlf)
   )
   (PUBLICIDAD nil)
   (DESPEDIDA nil)
   (reset)
)

; ===========================
; PROBLEMAS CON EL MONITOR
; ===========================
(defrule PROBLEMA-CON-MONITOR
   ?hecho <- (problema-bios)
   ?hecho_actual <- (EQUIPO (v_pantalla_enciende No))
=>
- La pantalla enciende pero deja un fondo oscuro y no muestra imagen? <NO>
   (printout t "LA PANTALLA ENCIENDE, PERO DEJA UN FONDO OSCURO Y NO MUESTRA IMAGEN? (Si/No)" crlf)
   (bind ?pantalla_oscura (read))
   (printout t "" crlf)
   (if (eq ?pantalla_oscura No)
      then
         (assert (pantalla-no-enciende))
      else
	     (assert (problemas-procesador))
   )
   (retract ?hecho)
)

(defrule PANTALLA-NO-ENCIENDE
   ?hecho <- (pantalla-no-enciende)
=>
   (printout t "NOTAS QUE EL EQUIPO FUNCIONA, MAS LA PANTALLA PARECE ESTAR APAGADA O DESCONECTADA" crlf)
   (printout t "LUEGO DE RECIEN ENCENDER EL EQUIPO, ESTE LANZA BEEPS (O PITIDOS) EN SENAL DE QUE ALGO NO ESTA BIEN" crlf)
   (printout t "Y LUEGO SE APAGA DE FORMA INMEDIATA?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
         (assert (remplaza-ram))
;	  else
;	     PROGRAMAR QUE SI ENTRA A WINDOWS O LO QUE SEA
   )
)

(defrule REMPLAZA-RAM
   ?hecho <- (remplaza-ram)
=>
   (printout t "HAS INTENTADO CAMBIAR LA MEMORIA RAM DE TU EQUIPO?" crlf)
   (printout t "O HAS PEDIDO A ALGUN TECNICO PARA QUE REALICE UN TEST DE TUS TARJETAS RAM?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor No)
      then
         (printout t "R// ES PROBABLE QUE TU EQUIPO TENGA PROBLEMAS DE RAM," crlf)
		 (printout t "TE RECOMENDAMOS HABLAR CON UN TECNICO Y REALIZAR UN TEST DE TUS TARJETAS RAM" crlf)
		 (PUBLICIDAD nil)
		 (DESPEDIDA nil)
		 (reset)
	  else
	     (printout t "R// LOS PROBLEMAS DE TU EQUIPO PARECEN ESTAR ASOCIADOS A PROBLEMAS DE MOTHERBOARD," crlf)
		 (PUBLICIDAD nil)
		 (DESPEDIDA nil)
		 (reset)
   )
)

(defrule LUZ-LECTURA-ESCRITURA
   ?hecho <- (luz-lectura-escritura)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
=>
   (if (eq ?tipo Portatil)
      then
         (printout t "LAS COMPUTADORAS PORTATILES CUENTAN CON AL MENOS 3 LUCES QUE INDICAN EL ESTADO DE TU PC" crlf)
	     (printout t "1. LA PRIMERA INDICA SI EL EQUIPO ESTA ENCENDIDO O NO, SIEMPRE ENCENDIDO Y SIN PARPADEAR" crlf)
	     (printout t "2. OTRA QUE INDICA SI EL CARGADOR ESTA CONECTADO, SE ENCIENDE Y APAGA AL DESCONECTAR EL CARGADOR" crlf)
	     (printout t "3. Y UNA MAS QUE SE MANTIENE INTERMITENTE, INDICANDO QUE EL EQUIPO TIENE ACTIVIDAD" crlf)
	  else
	     (printout t "LOS EQUIPOS DE ESCRITORIO CUENTAN CON AL MENOS 2 LUCES QUE INDICAN EL ESTADO DE TU PC" crlf)
	     (printout t "1. LA PRIMERA INDICA SI EL EQUIPO ESTA ENCENDIDO O NO, SIEMPRE ENCENDIDO Y SIN PARPADEAR" crlf)
	     (printout t "2. Y UNA MAS QUE SE MANTIENE INTERMITENTE, INDICANDO QUE EL EQUIPO TIENE ACTIVIDAD" crlf)
   )
   (printout t "LOGRAS APRECIAR SI TU EQUIPO TIENE ACTIVIDAD?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
         (printout t "ESPEREMOS 5 MINUTOS, A FIN DE ASUMIR QUE EL EQUIPO ESTA FUNCIONANDO CON NORMALIDAD" crlf)
         (assert (pruebas-monitor))
   )
   (retract ?hecho)
)

; ===========================
; PROBLEMAS CON EL MONITOR
; ===========================
(defrule PRUEBAS-MONITOR-PORTATIL
   ?hecho <- (pruebas-monitor)
   ?hecho_actual <- (EQUIPO (v_tipo Portatil))
=>
   (printout t "LOS EQUIPOS PORTATILES CUENTAN CON AL MENOS 1 SALIDA PARA CONECTAR UN MONITOR EXTERNO" crlf)
   (printout t "EL SIGUIENTE PASO PUEDE SER COMPLICADO POR CARENCIA DE MATERIALES" crlf)
   (printout t "NECESITAS UN MONITOR DISPONIBLE, Y REALIZAR UNA PRUEBA" crlf)
   (printout t "TIENES A TU ALCANCE UN MONITOR DISPONIBLE?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
         (assert (cambia-monitor))
	  else
	     (printout t "ENTENDEMOS TU DIFICULTAD, TRATA DE CONSEGUIR UN MONITOR QUE FUNCIONE E INTENTA MAS TARDE" crlf)
		 (PUBLICIDAD nil)
		 (DESPEDIDA nil)
		 (reset)
   )
   (retract ?hecho)
)

(defrule CAMBIA-MONITOR
   ?hecho <- (cambia-monitor)
=>
   (printout t "INTENTA CONECTAR EL MONITOR DISPONIBLE Y VERIFICA EL RESULTADO" crlf)
   (printout t "TOMATE TU TIEMPO, SABEMOS QUE PUEDE SER UNA TAREA DIFICIL, PRESIONA OK AL TERMINAR" crlf)
   (read)
   (printout t "EL MONITOR MOSTRO IMAGENES AL CONECTAR?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
         (printout t "R// TU MONITOR DEJO DE FUNCIONAR, NECESITAS LLEVAR TU EQUIPO A UN CHEQUEO," crlf)
		 (PUBLICIDAD nil)
		 (DESPEDIDA nil)
		 (reset)
	  else
	     (printout t "PROBABLEMENTE TENGAS PROBLEMAS CON TU TARJETA DE VIDEO, O BIEN TU MOTHERBOARD")
		 (PUBLICIDAD nil)
		 (DESPEDIDA nil)
		 (reset)
   )
)

(defrule PRUEBAS-MONITOR-ESCRITORIO
   ?hecho <- (pruebas-monitor)
   ?hecho_actual <- (EQUIPO (v_tipo Escritorio))
=>
   (assert (cable-poder-monitor))
   (retract ?hecho)
)

(defrule CABLE-PODER-AVERIADO-MONITOR
   ?hecho <- (cable-poder-monitor)
=>
   (printout t "EL SIGUIENTE PASO PUEDE SER COMPLICADO POR CARENCIA DE MATERIALES" crlf)
   (printout t "NECESITAS OTRO CABLE CON LAS MISMAS CARACTERISTICAS PARA ALIMENTAR CON CORRIENTE A TU MONITOR" crlf)
   (printout t "PUEDES CONSEGUIR UN CABLE Y VERIFICAR EL COMPORTAMIENTO?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
	     (assert (cable-poder-monitor-funciona))
	  else
	     (printout t "ENTENDEMOS TU DIFICULTAD, TRATA DE CONSEGUIR UN CABLE SIMILAR E INTENTA MAS TARDE" crlf)
		 (PUBLICIDAD nil)
		 (DESPEDIDA nil)
		 (reset)
   )
   (retract ?hecho)
)

(defrule CABLE-PODER-MONITOR-FUNCIONA
   ?hecho <- (cable-poder-monitor-funciona)
   ?hecho_actual <- (EQUIPO (v_tipo ?tipo))
   ?arranque <- (arranque)
=>
   (printout t "REMPLAZA EL CABLE DE PODER E INTENTA VER QUE SUCEDE" crlf)
   (printout t "TOMATE TU TIEMPO..., Y ESCRIBE OK AL HABER CULMINADO ESTA ACCION" crlf)
   (read)
   (printout t "" crlf)
   (printout t "GRACIAS POR INTENTARLO" crlf)
   (printout t "EL MONITOR MOSTRO IMAGEN AL INTENTAR CON OTRO CABLE DE PODER?" crlf)
   (RESP_SI_NO nil)
   (bind ?valor (ASIGNA_RESP (read)))
   (printout t "" crlf)
   (if (eq ?valor Si)
      then
         (printout t "R// TIENES PROBLEMAS CON EL CARGADOR DE TU MONITOR, TE RECOMENDAMOS COMPRAR UNO NUEVO" crlf)
         (PUBLICIDAD nil)
         (DESPEDIDA nil)
         (reset)
      else
         (printout t "EL SIGUIENTE PASO PUEDE SER COMPLICADO POR CARENCIA DE MATERIALES" crlf)
         (printout t "NECESITAS UN MONITOR DISPONIBLE, Y REALIZAR UNA PRUEBA" crlf)
         (printout t "TIENES A TU ALCANCE UN MONITOR DISPONIBLE?" crlf)
		 (RESP_SI_NO nil)
         (bind ?disponible (ASIGNA_RESP (read)))
		 (printout t "" crlf)
         (if (eq ?disponible Si)
            then
               (assert (cambia-monitor))
            else
               (printout t "ENTENDEMOS TU DIFICULTAD, TRATA DE CONSEGUIR UN MONITOR QUE FUNCIONE E INTENTA MAS TARDE" crlf)
               (PUBLICIDAD nil)
               (DESPEDIDA nil)
               (reset)
         )
   )
)

; ==========================
; PROBLEMAS CON PROCESADOR
; ==========================
(defrule PROBLEMAS-PROCESADOR
   ?hecho <- (problemas-procesador)
=>
   (assert (lentitud-equipo))
)

; ==================
; RESULTADOS
; ==================
(defrule EQUIPO-ENCIENDE
   (EQUIPO (v_enciende Si))
   (ENCIENDE (v_enciende Si))
; (CORREGIDO ?corregido&:(= 1 ?*corregido*))
=>
   (printout t ?*mensaje* crlf)
   (DESPEDIDA nil)
; (reset)
)

;(defrule DESPEDIDA
;   (despedida)
;=>
;   (printout t "GRACIAS POR USAR NUESTRO SISTEMA" crlf "SALUDOS" crlf)
;   (printout t "=================================================================" crlf)
;)
