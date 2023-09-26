#lang eopl

; Ejercicio 1

; Juan Sebastian Cifuentes Vallejo - 202179800
; Maria Alejandra Carvajal Perez - 202178495
; Yissy Katherine Posso Perea - 202181910

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Gramatica BNF:
;
; <expresion-FNC> ::= FNC <entero> <conjuncion-de-clausulas>
;
; <conjuncion-de-clausulas> ::= (<clausula>)
;                           ::= (<clausula> and <conjuncion-de-clausulas>)
;
; <clausula> ::= (<variable>)
;            ::= (<variable> or <clausula>)
;
; <variable> ::= <entero-diferente-a-cero>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; <variable> ::= <entero-diferente-a-cero>
(define variable
  (lambda (entero)
    entero
  )
)

; <clausula> ::= (<variable>)
(define or-var
  (lambda (var)
    (cons var '())
  )
)

; <clausula> ::= (<variable> or <clausula>)
(define or-var-clau
  (lambda (var clau)
    (cons var (cons 'or clau))
  )
)

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

; <expresion-FNC> ::= FNC <entero> <conjuncion-de-clausulas>
(define fnc
  (lambda (ent conj-clau)
    (cons 'FNC (cons ent (cons conj-clau '())))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
