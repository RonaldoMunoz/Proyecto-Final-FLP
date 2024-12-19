;; Autores: Yeifer Ronaldo Muñoz 2278665, Juan Carlos Rojas Quintero 2359358, Michael Steven Rodriguez Arana

#lang eopl
(define especificacion-lexica
  '(
    (espacio-blanco (whitespace) skip)
    (comentario ("*" (arbno (not #\newline)) "*") skip)
    (identificador (letter (arbno (or letter digit "?" "$"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit)"." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)"." digit (arbno digit)) number)
    (caracter ( "\'" letter "\'") symbol)
    (cadena ("\""(or letter digit) (arbno (or letter digit "?" "$"))"\"") string)
    )
  )
 
;; Especificación gramatical que define que tipo de expresiones y otros elementos existen en el interpretador. 
(define especificacion-gramatical
  '(
    (programa (expresion) a-program)
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) id-exp)
    (expresion (numero) num-exp)
    (expresion (caracter) carac-exp)
    (expresion (cadena) string-exp)
    (expresion ("ok") ok-exp)
    (expresion (primitiva "(" (separated-list expresion ",") ")") exp-primitiva)
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion "end") var-exp)
    (expresion ("let" (separated-list identificador "=" expresion ",") "in" expresion "end") let-exp)
    (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) "in" expresion "end") letrec-exp)
    (expresion ("set" identificador ":=" expresion) set-exp)
    (expresion ("begin" expresion (separated-list expresion ",") "end") begin-exp)
    (expresion ("if" bool-expresion "then" expresion (arbno "elseif" bool-expresion "then" expresion) "else" expresion "end") if-exp)
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion "end") proc-exp)
    (expresion ("apply" identificador "(" (separated-list expresion ",") ")") apply-exp)
    (expresion ("meth" "(" identificador "," (separated-list identificador ",") ")" expresion "end") meth-exp)
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "end") for-exp)
    (expresion ("object" "{" (arbno identificador "=>" expresion) "}" ) object-exp)
    (expresion ("get" identificador "." identificador) get-exp)
    (expresion ("send" identificador "." identificador "(" (separated-list expresion ",") ")") send-exp)
    (expresion ("update" identificador "." identificador ":=" expresion) update-exp)
    (expresion ("clone" "(" identificador (separated-list identificador ",") ")" ) clone-exp)
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)
    (bool-expresion ( bool-primitiva "(" (separated-list expresion ",") ")" ) bool-prim)
    (bool-expresion ( bool-oper "(" (separated-list bool-expresion ",") ")") bool-operation)
    (bool-primitiva (">") greater-prim)
    (bool-primitiva ("<") lesser-prim)
    (bool-primitiva ("<=") lesser-or-equal-prim)
    (bool-primitiva (">=") greater-or-equal-prim)
    (bool-primitiva ("is") is-prim)
    (bool-oper ("and") and-prim)
    (bool-oper ("or") or-prim)
    (bool-oper ("not") not-prim)
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") mod-prim)
    (primitiva ("&") txt-prim)
    
    )
  )
 
;;Creamos los datatypes que se van a utilizar en el interpretador 
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

 
 
;;Evaluamos el programa
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (a-program (exp)
                 (evaluar-expresion exp ambiente-inicial))
      ))
  )
 
 
 
;; Ambiente

(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido-ref
   (lids (list-of symbol?))
   (lvalue vector?)
   (old-env ambiente?)))


;;ambiente-extendido
(define ambiente-extendido
  (lambda (lids lvalue old-env)
    (ambiente-extendido-ref lids (list->vector lvalue) old-env)))

 
;; ambiente-extendido-recursivo
(define ambiente-extendido-recursivo
  (lambda (procnames lidss cuerpos old-env)
    (let
        (
         (vec-clausuras (make-vector (length procnames)))
         )
      (letrec
          (
           (amb (ambiente-extendido-ref procnames vec-clausuras old-env))
           (obtener-clausuras
            (lambda (lidss cuerpos pos)
              (cond
                [(null? lidss) amb]
                [else
                 (begin
                   (vector-set! vec-clausuras pos
                                (closure (car lidss) (car cuerpos) amb))
                   (obtener-clausuras (cdr lidss) (cdr cuerpos) (+ pos 1)))]
                )
              )
            )
           )
        (obtener-clausuras lidss cuerpos 0)
        )
      )
    )
  )


 (define apply-env
  (lambda (env var)
    (deref (apply-env-ref env var))))


(define apply-env-ref
  (lambda (env var)
    (cases ambiente env
      (ambiente-vacio () (eopl:error "No se encuentra la variable " var))
      (ambiente-extendido-ref (lid vec old-env)
                          (letrec
                              (
                               (buscar-variable (lambda (lid vec pos)
                                                  (cond
                                                    [(null? lid) (apply-env-ref old-env var)]
                                                    [(equal? (car lid) var) (a-ref pos vec)]
                                                    [else
                                                     (buscar-variable (cdr lid) vec (+ pos 1)  )]
                                                    )
                                                  )
                                                )
                               )
                            (buscar-variable lid vec 0)
                            )
                          
                          )
      
      )
    )
  )


;;Definimos el ambiente inicial
(define ambiente-inicial
  (ambiente-vacio))

;;Evaluar expresion


;; evaluar-primitiva
(evaluar-primitiva
  (lambda (prim lista)
    (cases primitiva prim
      (sum-prim () (operar lista + 0))
      (minus-prim () (- (car lista) (cadr lista)))
      (mult-prim () (operar lista * 1))
      (div-prim () (operar lista / 1))
      (mod-prim () (modulo (car lista) (cadr lista)))
      (txt-prim () (string-append (car lista)(cadr lista)))
      (else 0))))
 
 
;;Interpretador
(define interpretador
  (sllgen:make-rep-loop "-->" evaluar-programa
                        (sllgen:make-stream-parser
                         especificacion-lexica especificacion-gramatical)))
 
 
 
 
 
(interpretador)