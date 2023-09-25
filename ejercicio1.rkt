#lang eopl
; Ejercicio 1

; Juan Sebastian Cifuentes Vallejo - 202179800
; Maria Alejandra Carvajal Perez - 202178495
; Yissy Katherine Posso Perea - 202181910

; Gramatica BNF:
;
; <expresion-FNC> ::= FNC <entero-positivo> (<conjuncion-de-clausulas>)
;
; <conjuncion-de-clausulas> ::= (<clausula>)
;                           ::= (<clausula>) and <conjuncion-de-clausulas>
;
; <clausula> ::= <variable>
;            ::= <variable> or <clausula>
;
; <variable> ::= <entero-positivo>
;            ::= -<entero-positivo>
