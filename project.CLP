; Definiendo una variable global que contendra la puntuacion final
(defglobal 
    ?*finalpoint* = 0
)

(deftemplate node
   (slot name)
   (slot question)
   (slot A)
   (slot B)
   (slot C)
   (slot D)
   (slot next-question) 	
)

(deftemplate typeValue
 (slot typeName)
 (slot value)
 (slot personalityType)
)


(deffacts inicial
(node (name Q1) (question "Pregunta 1") (A "Gustar Autoridad") (B "Entusiasmo") (C "Sensible") (D "Gustar Instrucciones") (next-question Q2))
(node (name Q2) (question "Pregunta 2") (A "Hacerse Cargo") (B "Tomar Riesgos") (C "Leal") (D "Preciso") (next-question Q3))
(node (name Q3) (question "Pregunta 3") (A "Determinado") (B "Visionario") (C "Calmado") (D "Consistente") (next-question Q4))
(node (name Q4) (question "Pregunta 4") (A "Emprendedor") (B "Muy Verbal") (C "Gustar Rutina") (D "Predicible") (next-question Q5))
(node (name Q5) (question "Pregunta 5") (A "Competitivo") (B "Promotor") (C "Rechazo al Cambio") (D "Practico") (next-question Q6))
(node (name Q6) (question "Pregunta 6") (A "Solucionador") (B "Gustar Popularidad") (C "Obediente") (D "Factico") (next-question Q7))
(node (name Q7) (question "Pregunta 7") (A "Productivo") (B "Amante-Diversion") (C "Evita Confrontaciones") (D "Concienzudo") (next-question nil))

(typeValue (typeName Lider) (value 0) (personalityType "lider")) 
(typeValue (typeName Extrovertido) (value 0) (personalityType "extrovertido")) 
(typeValue (typeName Calmado) (value 0) (personalityType "calmada")) 
(typeValue (typeName Practico) (value 0) (personalityType "practico"))	
(currentQ Init))


(defrule InitialPersonList   
   ?Q <- (currentQ Init) 	
=>
   (printout t "*************************************************" crlf)
   (printout t "* Proyecto CLIPS: Krish Sadhwani, Javier Garcia *" crlf)
   (printout t "*************************************************" crlf)
   (retract ?Q)
   (assert (currentQ Q1))
)


(deffunction Value-One-Option(?option)
   (printout t ?option " (selecciona del 1...4) "crlf)
   (bind ?answer (read))                      
   (while (and (integerp ?answer)(or (< ?answer 1) (> ?answer 4))) 
     (printout t ?option " (selecciona del 1...4) "crlf)
     (bind ?answer (read)))                      
   (return ?answer))   

(defrule askQ   
   ?f1 <- (currentQ ?currentNode)
   (node (name ?currentNode) (question ?Q) (A ?A) (B ?B) (C ?C) (D ?D) (next-question ?nextNode))
   ?fA <- (typeValue (typeName Lider) (value ?AVal))
   ?fB <- (typeValue (typeName Extrovertido) (value ?BVal))
   ?fC <- (typeValue (typeName Calmado) (value ?CVal))
   ?fD <- (typeValue (typeName Practico) (value ?DVal))
=>
   (printout t ?Q ") Pon el numero '4' al lado de la palabra que mejor te describa; un '3' al lado de la siguiente palabra
que mejor te describa ; un '2' a la siguiente mejor, y un '1' a la palabra que menos te describa. " crlf)
   (printout t "___" ?A crlf)
   (printout t "___" ?B crlf)
   (printout t "___" ?C crlf)
   (printout t "___" ?D crlf)
   (printout t "*********************" crlf)

   (bind ?AAns (Value-One-Option ?A))                      
   (bind ?BAns (Value-One-Option ?B))                      
   (bind ?CAns (Value-One-Option ?C))                      
   (bind ?DAns (Value-One-Option ?D))                      


   ;(bind ?nextNode (ValidateResponses ?AAns ?BAns ?CAns ?DAns ?currentNode ?nextNode))                      

   (if (neq ?nextNode ?currentNode) then
  (modify ?fA (value (+ ?AVal ?AAns)))
  (modify ?fB (value (+ ?BVal ?BAns)))
  (modify ?fC (value (+ ?CVal ?CAns)))
  (modify ?fD (value (+ ?DVal ?DAns)))
    )
    
   (retract ?f1)
   (assert (currentQ ?nextNode))   
)


(defrule findAns   
   ?f1 <- (currentQ nil)
   ?fA <- (typeValue (typeName Lider) (value ?aVal))
   ?fB <- (typeValue (typeName Extrovertido) (value ?bVal))
   ?fC <- (typeValue (typeName Calmado) (value ?cVal))
   ?fD <- (typeValue (typeName Practico) (value ?dVal)) 	  
=>
   (retract ?f1)
   (assert (currentQ sort))	
   (assert (personalityList ?fA ?fB ?fC ?fD))
   
   (printout t "Resultado: " crlf)
   (printout t "Lider: " ?aVal crlf)
   (printout t "Extrovertido: " ?bVal crlf)
   (printout t "Calmado: " ?cVal crlf)
   (printout t "Practico: " ?dVal crlf)

   ; Para la variable global definida, se le adjunta el valor total de las variables con bind:

   (bind ?*finalpoint* (+ ?aVal ?bVal ?cVal ?dVal))
   ; (printout t ?*finalpoint* crlf)

   ; Si el total es 70, se ha hecho la encuesta correctamente, y sino se le pide que vuelve a realizarla:

   (if (> ?*finalpoint* 70) then (printout t "ERROR: NO SE PUEDEN REPETIR LOS NUMEROS EN LAS RESPUESTAS. VUELVA A REALIZAR EL TEST." crlf))
   (if (< ?*finalpoint* 70) then (printout t "ERROR: NO SE PUEDEN REPETIR LOS NUMEROS EN LAS RESPUESTAS. VUELVA A REALIZAR EL TEST." crlf))
   (if (eq ?*finalpoint* 70) then (printout t "CORRECTO. ESTA ES LA CONCLUSION SACADA:" crlf))

)

(defrule evaluate_max
    ; Se evalua el maximo valor para cada campo de los resultados:

    (typeValue (typeName ?id) (value ?value))
    (not (typeValue (value ?nvalue&:(> ?nvalue ?value))))
=>
    (printout t "Puntuacion maxima es ---> " ?id crlf)

	; Si la puntuacion final es 70 y dependiendo de que campo (lider, extrovertida, calmada, práctica) es el maximo, se printea la conclusion:

	(if (and (eq ?id Lider) (eq ?*finalpoint* 70)) then (printout t "Eres una persona lider. Te caracterizas por tu gran capacidad de motivacion y comunicacion,
	    eres capaz de resolver situaciones dificiles con gran rapidez y no tienes problemas a la hora de llevar la iniciativa de un trabajo o un proyecto. Este tipo
	    de personas son muy aptas para crear vinculos y siempre estan buscando nuevos estimulos y metas que lograr." crlf))

    (if (and (eq ?id Extrovertido) (eq ?*finalpoint* 70)) then (printout t "Eres una persona extrovertida. Llama la atencion la impulsividad con la que tomas decisiones y la impaciencia a la hora de conseguir objetivos,
	    tienes una gran habilidad para crear nuevas relaciones sociables y eres una persona con mucha energia y positividad, aunque en ocasiones presentas casos de
	    agresividad o despreocupacion." crlf))

	(if (and (eq ?id Calmado) (eq ?*finalpoint* 70)) then (printout t "Eres una persona calmada. Presentas mucha tranquilidad ante cualquier situacion y a la hora de tomar decisiones lo haces de una manera
	    muy racional y paciente sin dejarte llevar por impulsos, no te preocupa demasiado hacer tareas rapidamente y sueles proponerte objetivos a muy largo plazo,
	    aparte, suelen ser personas calladas y que les cuesta establecer nuevas relaciones sociales." crlf))

    (if (and (eq ?id Practico) (eq ?*finalpoint* 70)) then (printout t "Eres una persona practica. Llevas sin agresion el recibir ordenes de los demas, eres una persona muy paciente que no le importa
	    escuchar las indicaciones de sus companeros y ejecutarlas, siendo sencillo trabajar en grupo, ademas, son muy metodicas y constantes, buscando siempre
	    la solucion mas eficiente y rapida, aparte de resolver con excelencia imprevistos que puedan surgir." crlf))

    
)

