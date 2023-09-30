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
; Constructores or

; <clausula> ::= (<variable-o--variable>)
(define or-var
  (lambda (var-o--var)
    (cons var-o--var '())
  )
)

; <clausula> ::= (<variable-o--variable> or <clausula>)
(define or-var-clau
  (lambda (var-o--var clau)
    (cons var-o--var (cons 'or clau))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constructores and

; <conjuncion-de-clausulas> ::= (<clausula>)
(define and-clau
  (lambda (clau)
    (cons clau '())
  )
)

; <conjuncion-de-clausulas> ::= (<clausula> and <conjuncion-de-clausulas>)
(define and-clau-conj
  (lambda (clau conj)
    (cons clau (cons 'and conj))
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
#|
(define or->varlist
  (lambda (or-)
    (cond
      [(null? or-) '()]
      [(#|entero-no-cero?|#integer? (car or-)) (cons (car or-) (or->varlist (cdr or-)))]
      [else (or->varlist (cdr or-))]
    )
  )
)|#

(define or->varlist
  (lambda (or-)
    (cond
      [(null? or-) '()]
      [(not (number? (car or-))) (or->varlist (cdr or-))]
      [(> (car or-) 0) (cons (car or-) (or->varlist (cdr or-)))]
      [else (cons (* (car or-) -1) (or->varlist (cdr or-)))]
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

(define clausula_1 (or-var variable_1))
(define clausula_2 (or-var-clau variable_2 clausula_1))
(define clausula_3 (or-var-clau variable_-3 clausula_2))
(define clausula_4 (or-var variable_-1))
(define clausula_5 (or-var-clau variable_-2 clausula_4))
(define clausula_6 (or-var-clau variable_3 clausula_5))

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

(define conjuncion_1 (and-clau clausula_1))
(define conjuncion_2 (and-clau-conj clausula_3 conjuncion_1))
(define conjuncion_3 (and-clau-conj clausula_5 conjuncion_2))
(define conjuncion_4 (and-clau clausula_2))
(define conjuncion_5 (and-clau-conj clausula_4 conjuncion_4))
(define conjuncion_6 (and-clau-conj clausula_6 conjuncion_5))

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

; Auxiliar
(define simbolo-or?
  (lambda (simboloXD)
    (eqv? simboloXD 'or)
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
  (v-variable-o--variable-or-clausula (c-variable-o--variable t-variable-o--variable?) (or simbolo-or?) (c-clausula t-clausula?))
)

; <conjuncion-de-clausulas> ::= (<clausula>)
; <conjuncion-de-clausulas> ::= (<clausula> and <conjuncion-de-clausulas>)
;(define-datatype -conjuncion-de-clausulas conjuncion-de-clausulas?
;  (clausula- ())
;)

; <expresion-FNC> ::= FNC <entero> <conjuncion-de-clausulas>

(define-datatype expresion-FNC expresion-FNC?
  (FNC (entero integer?))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utilización y creación de por lo menos 3 instancias SAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define t-variable_1 (v-entero-diferente-a-cero 1))
(define t-variable_2 (v-entero-diferente-a-cero 2))
(define t-variable_3 (v-entero-diferente-a-cero 3))
(define t-variable_4 (v-entero-diferente-a-cero -1))
(define t-variable_5 (v-entero-diferente-a-cero -2))
(define t-variable_6 (v-entero-diferente-a-cero -3))

(newline)
(display t-variable_1)
(newline)
(display t-variable_2)
(newline)
(display t-variable_3)
(newline)
(display t-variable_4)
(newline)
(display t-variable_5)
(newline)
(display t-variable_6)
(newline)

;;;;;;;;;;;

(define t-clausula_1 (v-variable-o--variable t-variable_1))
(define t-clausula_2 (v-variable-o--variable-or-clausula t-variable_2 'or t-clausula_1))
(define t-clausula_3 (v-variable-o--variable-or-clausula t-variable_6 'or t-clausula_2))
(define t-clausula_4 (v-variable-o--variable t-variable_4))
(define t-clausula_5 (v-variable-o--variable-or-clausula t-variable_5 'or t-clausula_4))
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








