#lang eopl

; Ejercicio 2

; Juan Sebastian Cifuentes Vallejo - 202179800
; Maria Alejandra Carvajal Perez - 202178495
; Yissy Katherine Posso Perea - 202181910

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Función PARSEBNF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Función UNPARSEBNF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Implementación basada en datatypes
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
; UNPARSEBNF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

