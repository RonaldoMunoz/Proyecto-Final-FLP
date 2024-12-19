#lang racket/base

;; Autores: Yeifer Ronaldo Muñoz 2278665, Juan Carlos Rojas Quintero 2359358, Michael Steven Rodriguez Arana

(require rackunit "ProyectoFinalFLP.rkt")


;; Pruebas para primitivas
(define exp1 (scan&parse "+(2,3)"))
(define expected-exp1 5)
(check-equal? (evaluar-programa exp1) expected-exp1)

(define exp2 (scan&parse "-(5,2)"))
(define expected-exp2 3)
(check-equal? (evaluar-programa exp2) expected-exp2)

(define exp3 (scan&parse "*(2,3)"))
(define expected-exp3 6)
(check-equal? (evaluar-programa exp3) expected-exp3)

(define exp4 (scan&parse "/(10,2)"))
(define expected-exp4 5)
(check-equal? (evaluar-programa exp4) expected-exp4)

(define exp5 (scan&parse "%(10,3)"))
(define expected-exp5 1)
(check-equal? (evaluar-programa exp5) expected-exp5)

(define exp6 (scan&parse "&(\"Hola\",\" mundo\")"))
(define expected-exp6 "Hola mundo")
(check-equal? (evaluar-programa exp6) expected-exp6)

;; Pruebas para expresiones booleanas
(define exp7 (scan&parse ">(5,2)"))
(define expected-exp7 #t)
(check-equal? (evaluar-programa exp7) expected-exp7)

(define exp8 (scan&parse "<(2,5)"))
(define expected-exp8 #t)
(check-equal? (evaluar-programa exp8) expected-exp8)

(define exp9 (scan&parse ">=(5,5)"))
(define expected-exp9 #t)
(check-equal? (evaluar-programa exp9) expected-exp9)

(define exp10 (scan&parse "<=(5,5)"))
(define expected-exp10 #t)
(check-equal? (evaluar-programa exp10) expected-exp10)

(define exp11 (scan&parse "is(5,5)"))
(define expected-exp11 #t)
(check-equal? (evaluar-programa exp11) expected-exp11)

(define exp12 (scan&parse "and(true,false)"))
(define expected-exp12 #f)
(check-equal? (evaluar-programa exp12) expected-exp12)

(define exp13 (scan&parse "or(true,false)"))
(define expected-exp13 #t)
(check-equal? (evaluar-programa exp13) expected-exp13)

(define exp14 (scan&parse "not(true)"))
(define expected-exp14 #f)
(check-equal? (evaluar-programa exp14) expected-exp14)

;; Pruebas para control de flujo (if)
(define exp15 (scan&parse "if(>(5,2),then 10,else 20,end)"))
(define expected-exp15 10)
(check-equal? (evaluar-programa exp15) expected-exp15)

(define exp16 (scan&parse "if(<(2,5),then 10,elseif(>(2,5),then 20,else 30,end),else 30,end)"))
(define expected-exp16 10)
(check-equal? (evaluar-programa exp16) expected-exp16)

(define exp17 (scan&parse "if(<(2,5),then 10,elseif(<(2,5),then 20,else 30,end),else 30,end)"))
(define expected-exp17 10)
(check-equal? (evaluar-programa exp17) expected-exp17)

(define exp18 (scan&parse "if(false,then 10,elseif(false,then 20,else 30,end),else 30,end)"))
(define expected-exp18 30)
(check-equal? (evaluar-programa exp18) expected-exp18)

;; Pruebas para var, let y letrec
(define exp19 (scan&parse "var x = 10, y = 20 in +(x,y) end"))
(define expected-exp19 30)
(check-equal? (evaluar-programa exp19) expected-exp19)

(define exp20 (scan&parse "let x = 10, y = 20 in +(x,y) end"))
(define expected-exp20 30)
(check-equal? (evaluar-programa exp20) expected-exp20)

(define exp21 (scan&parse "letrec f(x) = if(is(x,0),then 1,else *(x,(f(-(x,1))),end) in (f 5) end"))
(define expected-exp21 120)
(check-equal? (evaluar-programa exp21) expected-exp21)

;; Pruebas para set y begin
(define exp22 (scan&parse "var x = 10 in begin set x := 20, x end end"))
(define expected-exp22 20)
(check-equal? (evaluar-programa exp22) expected-exp22)

(define exp23 (scan&parse "begin 1,2,3 end"))
(define expected-exp23 3)
(check-equal? (evaluar-programa exp23) expected-exp23)

;; Pruebas para procedimientos (proc y apply)
(define exp24 (scan&parse "proc(x,y) +(x,y) end"))
(define expected-exp24 (closure '(x y) '(+ x y) (ambiente-vacio)))
(check-equal? (evaluar-programa exp24) expected-exp24)

(define exp25 (scan&parse "apply(proc(x,y) +(x,y) end,(1,2))"))
(define expected-exp25 3)
(check-equal? (evaluar-programa exp25) expected-exp25)

;; Pruebas para métodos (meth)
(define exp26 (scan&parse "meth(self,x) +(self.a,x) end"))
(define expected-exp26 (closure '(self x) '(+ self.a x) (ambiente-vacio)))
(check-equal? (evaluar-programa exp26) expected-exp26)

;; Pruebas para objetos (object, get, send, update)
(define exp27 (scan&parse "object {}"))
(define expected-exp27 (object-empty))
(check-equal? (evaluar-programa exp27) expected-exp27)

(define exp28 (scan&parse "object {a => 10, b => 20}"))
(define expected-exp28 (object-atributes 'a 10 (object-atributes 'b 20 (object-empty))))
(check-equal? (evaluar-programa exp28) expected-exp28)

(define exp29 (scan&parse "var obj = (object {a => 10}) in (get obj.a) end"))
(define expected-exp29 10)
(check-equal? (evaluar-programa exp29) expected-exp29)

(define exp30 (scan&parse "var obj = (object {a => 10, m => (meth(self,x) +(self.a,x) end)}) in (send obj.m,(5)) end"))
(define expected-exp30 15)
(check-equal? (evaluar-programa exp30) expected-exp30)

(define exp31 (scan&parse "var obj = (object {a => 10}) in begin update obj.a := 20, (get obj.a) end end"))
(define expected-exp31 20)
(check-equal? (evaluar-programa exp31) expected-exp31)

;; Pruebas para clone
(define exp32 (scan&parse "var obj1 = (object {a => 10}) in var obj2 = (clone (obj1)) in (get obj2.a) end end"))
(define expected-exp32 10)
(check-equal? (evaluar-programa exp32) expected-exp32)

;; Pruebas para ciclos (for)
(define exp33 (scan&parse "for i = 1 to 5 do +(i,i) end"))
(define expected-exp33 10)
(check-equal? (evaluar-programa exp33) expected-exp33)
