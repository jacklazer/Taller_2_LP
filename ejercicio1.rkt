#lang eopl

; Ejercicio 1

; Juan Sebastian Cifuentes Vallejo - 202179800
; Maria Alejandra Carvajal Perez - 202178495
; Yissy Katherine Posso Perea - 202181910

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Gramática BNF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Gramatica BNF:
;
; <expresion-FNC> ::= FNC <entero> <conjuncion-de-clausulas>
;
; <conjuncion-de-clausulas> ::= (<clausula>)
;                           ::= (<clausula> and <conjuncion-de-clausulas>)
;
; <clausula> ::= (<variable-o--variable>)
;            ::= (<variable-o--variable> or <clausula>)
;
; <variable-o--variable> ::= <entero-diferente-a-cero>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Implementación de la gramática basada en listas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constructores (fnc, and, or)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Auxiliar y constructor de variable

; Auxiliar
(define entero-no-cero?
  (lambda (numeroXD)
    (and (integer? numeroXD) (not (eqv? numeroXD 0)))
  )
)

; <variable-o--variable> ::= <entero-diferente-a-cero>
(define variable-o--variable
  (lambda (entero)
    (cond
      [(entero-no-cero? entero) entero]
      [else "Error"]
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constructor or

; <clausula> ::= (<variable-o--variable>)
; <clausula> ::= (<variable-o--variable> or <clausula>)
(define or
  (lambda (var-o--var clau)
    (cond
      [(null? clau) (cons var-o--var clau)]
      [else (cons var-o--var (cons 'or clau))]
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constructores and

; <conjuncion-de-clausulas> ::= (<clausula>)
; <conjuncion-de-clausulas> ::= (<clausula> and <conjuncion-de-clausulas>)
(define and
  (lambda (clau conj)
    (cond
      [(null? conj) (cons clau conj)]
      [else (cons clau (cons 'and conj))]
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constructor fnc

; <expresion-FNC> ::= FNC <entero> <conjuncion-de-clausulas>
(define fnc
  (lambda (ent conj-clau)
    (cons 'FNC (cons ent (cons conj-clau '())))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Extractores (fnc − > var, fnc− >clausulas, or− > varlist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Extractor fnc->var

(define fnc->var
  (lambda (fnc-)
    (car (cdr fnc-))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Extractor fnc−>clausulas

(define aux-fnc->clausulas
  (lambda (conjuncion)
    (cond
      [(null? conjuncion) 0]
      [(list? (car conjuncion)) (+ 1 (aux-fnc->clausulas (cdr conjuncion)))]
      [else (+ 0 (aux-fnc->clausulas (cdr conjuncion)))]
    )
  )
)

(define fnc->clausulas
  (lambda (fnc-)
    (aux-fnc->clausulas (caddr fnc-))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Extractor or−>varlist

(define or->varlist
  (lambda (or-)
    (cond
      [(null? or-) '()]
      [(#|entero-no-cero?|# integer? (car or-)) (cons (car or-) (or->varlist (cdr or-)))]
      [else (or->varlist (cdr or-))]
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utilización y creación de por lo menos 3 instancias SAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define variable_1 (variable-o--variable 1))
(define variable_2 (variable-o--variable 2))
(define variable_3 (variable-o--variable 3))
(define variable_-1 (variable-o--variable -1))
(define variable_-2 (variable-o--variable -2))
(define variable_-3 (variable-o--variable -3))

(newline)
(display variable_1)
(newline)
(display variable_2)
(newline)
(display variable_3)
(newline)
(display variable_-1)
(newline)
(display variable_-2)
(newline)
(display variable_-3)
(newline)

;;;;;;;;;;;

(define clausula_1 (or variable_1 '()))
(define clausula_2 (or variable_2 clausula_1))
(define clausula_3 (or variable_-3 clausula_2))
(define clausula_4 (or variable_-1 '()))
(define clausula_5 (or variable_-2 clausula_4))
(define clausula_6 (or variable_3 clausula_5))

(newline)
(display clausula_1)
(newline)
(display clausula_2)
(newline)
(display clausula_3)
(newline)
(display clausula_4)
(newline)
(display clausula_5)
(newline)
(display clausula_6)
(newline)

;;;;;;;;;;;

(define conjuncion_1 (and clausula_1 '()))
(define conjuncion_2 (and clausula_3 conjuncion_1))
(define conjuncion_3 (and clausula_5 conjuncion_2))
(define conjuncion_4 (and clausula_2 '()))
(define conjuncion_5 (and clausula_4 conjuncion_4))
(define conjuncion_6 (and clausula_6 conjuncion_5))

(newline)
(display conjuncion_1)
(newline)
(display conjuncion_2)
(newline)
(display conjuncion_3)
(newline)
(display conjuncion_4)
(newline)
(display conjuncion_5)
(newline)
(display conjuncion_6)
(newline)

;;;;;;;;;;;

(define fnc_1 (fnc 1 conjuncion_1))
(define fnc_2 (fnc 3 conjuncion_2))
(define fnc_3 (fnc 3 conjuncion_3))
(define fnc_4 (fnc 2 conjuncion_4))
(define fnc_5 (fnc 2 conjuncion_5))
(define fnc_6 (fnc 3 conjuncion_6))

(newline)
(display fnc_1)
(newline)
(display fnc_2)
(newline)
(display fnc_3)
(newline)
(display fnc_4)
(newline)
(display fnc_5)
(newline)
(display fnc_6)
(newline)

;;;;;;;;;;;

(newline)
(display (fnc->var fnc_1))
(display "|")
(display (fnc->clausulas fnc_1))
(display "|")
(display (or->varlist (car (caddr fnc_1))))
(newline)
(display (fnc->var fnc_2))
(display "|")
(display (fnc->clausulas fnc_2))
(display "|")
(display (or->varlist (car (caddr fnc_2))))
(display "|")
(display (or->varlist (caddr (caddr fnc_2))))
(newline)
(display (fnc->var fnc_3))
(display "|")
(display (fnc->clausulas fnc_3))
(display "|")
(display (or->varlist (car (caddr fnc_3))))
(display "|")
(display (or->varlist (caddr (caddr fnc_3))))
(display "|")
(display (or->varlist (car (cddddr (caddr fnc_3)))))
(newline)
(display (fnc->var fnc_4))
(display "|")
(display (fnc->clausulas fnc_4))
(display "|")
(display (or->varlist (car (caddr fnc_4))))
(newline)
(display (fnc->var fnc_5))
(display "|")
(display (fnc->clausulas fnc_5))
(display "|")
(display (or->varlist (car (caddr fnc_5))))
(display "|")
(display (or->varlist (caddr (caddr fnc_5))))
(newline)
(display (fnc->var fnc_6))
(display "|")
(display (fnc->clausulas fnc_6))
(display "|")
(display (or->varlist (car (caddr fnc_6))))
(display "|")
(display (or->varlist (caddr (caddr fnc_6))))
(display "|")
(display (or->varlist (car (cddddr (caddr fnc_6)))))
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Implementación basada en datatypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Auxiliares
(define simbolo-or?
  (lambda (simboloXD)
    (eqv? simboloXD 'or)
  )
)

(define simbolo-and?
  (lambda (simboloXD)
    (eqv? simboloXD 'and)
  )
)

(define simbolo-FNC?
  (lambda (simboloXD)
    (eqv? simboloXD 'FNC)
  )
)

(define entero-mayor-a-cero?
  (lambda (numeroXD)
    (and (integer? numeroXD) (> numeroXD 0))
  )
)

; <variable-o--variable> ::= <entero-diferente-a-cero>
(define-datatype t-variable-o--variable t-variable-o--variable?
  (v-entero-diferente-a-cero (entero entero-no-cero?))
)

; <clausula> ::= (<variable-o--variable>)
; <clausula> ::= (<variable-o--variable> or <clausula>)
(define-datatype t-clausula t-clausula?
  (v-variable-o--variable (c-variable-o--variable t-variable-o--variable?))
  (v-variable-o--variable-or-clausula (c-variable-o--variable t-variable-o--variable?) (-or- simbolo-or?) (c-clausula t-clausula?))
)

; <conjuncion-de-clausulas> ::= (<clausula>)
; <conjuncion-de-clausulas> ::= (<clausula> and <conjuncion-de-clausulas>)
(define-datatype t-conjuncion-de-clausulas t-conjuncion-de-clausulas?
  (v-clausula (c-clausula t-clausula?))
  (v-clausula-conjuncion-de-clausulas (c-clausula t-clausula?) (-and- simbolo-and?) (c-conjuncion-de-clausulas t-conjuncion-de-clausulas?))
)

; <expresion-FNC> ::= FNC <entero> <conjuncion-de-clausulas>
(define-datatype t-expresion-FNC expresion-FNC?
  (v-expresion-FNC (-FNC- simbolo-FNC?) (-entero-mayo-a-cero- entero-mayor-a-cero?) (c-conjuncion-de-clausulas t-conjuncion-de-clausulas?))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utilización y creación de por lo menos 3 instancias SAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define t-variable_1 (v-entero-diferente-a-cero 1))
(define t-variable_2 (v-entero-diferente-a-cero 2))
(define t-variable_3 (v-entero-diferente-a-cero 3))
(define t-variable_-4 (v-entero-diferente-a-cero -1))
(define t-variable_-5 (v-entero-diferente-a-cero -2))
(define t-variable_-6 (v-entero-diferente-a-cero -3))

(newline)
(display t-variable_1)
(newline)
(display t-variable_2)
(newline)
(display t-variable_3)
(newline)
(display t-variable_-4)
(newline)
(display t-variable_-5)
(newline)
(display t-variable_-6)
(newline)

;;;;;;;;;;;

(define t-clausula_1 (v-variable-o--variable t-variable_1))
(define t-clausula_2 (v-variable-o--variable-or-clausula t-variable_2 'or t-clausula_1))
(define t-clausula_3 (v-variable-o--variable-or-clausula t-variable_-6 'or t-clausula_2))
(define t-clausula_4 (v-variable-o--variable t-variable_-4))
(define t-clausula_5 (v-variable-o--variable-or-clausula t-variable_-5 'or t-clausula_4))
(define t-clausula_6 (v-variable-o--variable-or-clausula t-variable_3 'or t-clausula_5))

(newline)
(display t-clausula_1)
(newline)
(display t-clausula_2)
(newline)
(display t-clausula_3)
(newline)
(display t-clausula_4)
(newline)
(display t-clausula_5)
(newline)
(display t-clausula_6)
(newline)

;;;;;;;;;;;

(define t-conjuncion_1 (v-clausula t-clausula_1))
(define t-conjuncion_2 (v-clausula-conjuncion-de-clausulas t-clausula_3 'and t-conjuncion_1))
(define t-conjuncion_3 (v-clausula-conjuncion-de-clausulas t-clausula_5 'and t-conjuncion_2))
(define t-conjuncion_4 (v-clausula t-clausula_2))
(define t-conjuncion_5 (v-clausula-conjuncion-de-clausulas t-clausula_4 'and t-conjuncion_4))
(define t-conjuncion_6 (v-clausula-conjuncion-de-clausulas t-clausula_6 'and t-conjuncion_5))

(newline)
(display t-conjuncion_1)
(newline)
(display t-conjuncion_2)
(newline)
(display t-conjuncion_3)
(newline)
(display t-conjuncion_4)
(newline)
(display t-conjuncion_5)
(newline)
(display t-conjuncion_6)
(newline)

;;;;;;;;;;;

(define t-fnc_1 (v-expresion-FNC 'FNC 1 t-conjuncion_1))
(define t-fnc_2 (v-expresion-FNC 'FNC 3 t-conjuncion_2))
(define t-fnc_3 (v-expresion-FNC 'FNC 3 t-conjuncion_3))
(define t-fnc_4 (v-expresion-FNC 'FNC 2 t-conjuncion_4))
(define t-fnc_5 (v-expresion-FNC 'FNC 2 t-conjuncion_5))
(define t-fnc_6 (v-expresion-FNC 'FNC 3 t-conjuncion_6))

(newline)
(display t-fnc_1)
(newline)
(display t-fnc_2)
(newline)
(display t-fnc_3)
(newline)
(display t-fnc_4)
(newline)
(display t-fnc_5)
(newline)
(display t-fnc_6)
(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Evaluación de Instancias SAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-list n value)
  (if (= n 0)
      '()
      (cons value (make-list (- n 1) value))))

(define (evaluar-clausula clausula asignacion)
  (cond
    [(null? clausula) #t] 
    [(list? clausula)
     (or (evaluar-clausula (car clausula) asignacion)
         (evaluar-clausula (cdr clausula) asignacion))]
    [else (and (eq? clausula (car asignacion))
               (evaluar-clausula '() (cdr asignacion)))]))

(define (evaluar-conjuncion conjuncion asignacion)
  (cond
    [(null? conjuncion) #t]
    [else (and (evaluar-clausula (car conjuncion) asignacion)
                (evaluar-conjuncion (cdr conjuncion) asignacion))]))

(define (EVALUARSAT instancia)
  (define valor-entero (fnc->var instancia))
  (define conjuncion-clausulas (caddr instancia))
  (define num-variables (aux-fnc->clausulas conjuncion-clausulas))

  (define (generar-asignaciones n)
    (if (= n 0)
        (list (make-list num-variables #f))
        (append (map (lambda (asignacion)
                       (cons #t asignacion))
                     (generar-asignaciones (- n 1)))
                (map (lambda (asignacion)
                       (cons #f asignacion))
                     (generar-asignaciones (- n 1))))))
  
  (define asignaciones (generar-asignaciones num-variables))

  (define (encontrar-satisfaccion asignaciones)
    (cond
      [(null? asignaciones) '()] 
      [else (let ((asignacion (car asignaciones)))
              (if (evaluar-conjuncion conjuncion-clausulas asignacion)
                  (cons 'satisfactible asignacion)
                  (encontrar-satisfaccion (cdr asignaciones))))]))

  (define resultado (encontrar-satisfaccion asignaciones))

  (if (eq? (car resultado) 'satisfactible)
      resultado
      '(insatisfactible '())))


(define instancia1 (fnc 2
  (list (list 1 2)
        (list -1)
        (list -2))))

(display (EVALUARSAT instancia1))






