#lang eopl

; Ejercicio 2

; Juan Sebastian Cifuentes Vallejo - 202179800
; Maria Alejandra Carvajal Perez - 202178495
; Yissy Katherine Posso Perea - 202181910

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Implementación basada en datatypes

; Auxiliares
(define entero-no-cero?
  (lambda (numeroXD)
    (and (integer? numeroXD) (not (eqv? numeroXD 0)))
  )
)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Función PARSEBNF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define PARSEBNF
  (lambda (expresion)
    (cond
      [(entero-no-cero? expresion) (v-entero-diferente-a-cero expresion)]
      [(entero-no-cero? (car expresion))
        (cond
          [(null? (cdr expresion)) (v-variable-o--variable (PARSEBNF (car expresion)))]
          [else (v-variable-o--variable-or-clausula (PARSEBNF (car expresion)) (cadr expresion) (PARSEBNF (cddr expresion)))]
        )
      ]
      [(list? (car expresion))
        (cond
          [(null? (cdr expresion)) (v-clausula (PARSEBNF (car expresion)))]
          [else (v-clausula-conjuncion-de-clausulas (PARSEBNF (car expresion)) (cadr expresion) (PARSEBNF (cddr expresion)))]
        )
      ]
          [(eqv? (car expresion) 'FNC) (v-expresion-FNC (car expresion) (cadr expresion) (PARSEBNF (caddr expresion)))]
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Arbol de sintaxis abstracta basado en listas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define arbol_de_sintaxis (PARSEBNF '(FNC 3 ((-2 or -1) and (-3 or 2 or 1) and (1)))))
(newline)
(display arbol_de_sintaxis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Función UNPARSEBNF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


