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
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Función UNPARSEBNF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Unparser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define UNPARSEBNF-variable-o--variable
  (lambda (t-var-o--var)
    (cases t-variable-o--variable t-var-o--var
      (v-entero-diferente-a-cero (entero) entero)
    )
  )
)

(define UNPARSEBNF-clausula
  (lambda (t-clau)
    (cases t-clausula t-clau
      (v-variable-o--variable (c-variable-o--variable) (UNPARSEBNF-variable-o--variable c-variable-o--variable))
      (v-variable-o--variable-or-clausula (c-variable-o--variable -or- c-clausula) (list (UNPARSEBNF-variable-o--variable c-variable-o--variable) -or- (UNPARSEBNF-clausula c-clausula)))
    )
  )
)

(define UNPARSEBNF-conjuncion-de-clausulas
  (lambda (t-con-de-clau)
    (cases t-conjuncion-de-clausulas t-con-de-clau
      (v-clausula (c-clausula) (UNPARSEBNF-clausula c-clausula))
      (v-clausula-conjuncion-de-clausulas (c-clausula -and- c-conjuncion-de-clausulas) (list (UNPARSEBNF-clausula c-clausula) -and- (UNPARSEBNF-conjuncion-de-clausulas c-conjuncion-de-clausulas)))
    )
  )
)

(define UNPARSEBNF
  (lambda (t-exp)
    (cases t-expresion-FNC t-exp
      (v-expresion-FNC (-FNC- -entero-mayo-a-cero- c-conjuncion-de-clausulas) (list -FNC- -entero-mayo-a-cero- (UNPARSEBNF-conjuncion-de-clausulas c-conjuncion-de-clausulas)))
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Representacion concreta basada en listas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define representacion_concreta (UNPARSEBNF (v-expresion-FNC 'FNC 3 (v-clausula-conjuncion-de-clausulas (v-variable-o--variable-or-clausula (v-entero-diferente-a-cero 3) 'or (v-variable-o--variable-or-clausula (v-entero-diferente-a-cero -2) 'or (v-variable-o--variable (v-entero-diferente-a-cero -1)))) 'and (v-clausula-conjuncion-de-clausulas (v-variable-o--variable (v-entero-diferente-a-cero -1)) 'and (v-clausula (v-variable-o--variable-or-clausula (v-entero-diferente-a-cero 2) 'or (v-variable-o--variable (v-entero-diferente-a-cero 1)))))))))
(newline)
(display representacion_concreta)
(newline)