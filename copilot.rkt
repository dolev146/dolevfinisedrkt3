#lang pl


(define-type FLANG
;; FLANG is the type of FLANG expressions
 [Num Number]
;; Num is the type of numeric values
 [Plus FLANG FLANG]
;; Plus is the type of addition expressions
 [Minus FLANG FLANG]
;; Minus is the type of subtraction expressions
 [Multipilication FLANG FLANG]
;; Multipilication is the type of Mult expressions
 [Div FLANG FLANG]
;; Div is the type of division expressions
 [With Symbol FLANG FLANG]
;; With is the type of with expressions
 [Id Symbol]
;; Id is the type of identifiers
 [Fun Symbol FLANG]
;; Fun is the type of function expressions 
 [Call FLANG FLANG]
;; Call is the type of function calls
 [Bool Boolean]
;; Bool is the type of Boolean values
 [Bigger FLANG FLANG]
;; Bigger is the type of greater-than expressions
 [Smaller FLANG FLANG]
;; Smaller is the type of smaller-than expressions
 [Equal FLANG FLANG]
;; Equal is the type of equal expressions
 [Not FLANG]
;; Not is the type of not expressions
 [If FLANG FLANG FLANG]
;; If is the type of if expressions
 )
 



(: parse-sexpr : Sexpr -> FLANG)
;; declare parse-sexpr function
 (define (parse-sexpr sexpr)
;; define parse-sexpr function
 (match sexpr
 ;; look for the following patterns in sexpr: number #t #f symbol and return the corresponding FLANG
 [(number: n) (Num n)]
 ['True (Bool true)]
 ['False (Bool false)]
 [(symbol: name) (Id name)]
 ;; if sexpr is a list, look for the following patterns in sexpr: + - * / 
 ;; with fun call = > < if not and return the corresponding FLANG
 [(cons 'with more)
  ( match sexpr
   [(list 'with (list (symbol: name) named-expr) body) (With name (parse-sexpr named-expr)(parse-sexpr body))]
   [else (error 'parse-sexpr "Invalid syntax for with")])]
 [(cons 'fun more)
  ( match sexpr
   [(list 'fun (list (symbol: name)) body)(Fun name (parse-sexpr body))]
   [else (error 'parse-sexpr "invalid syntax for fun")])]
 [(list '+ l r) (Plus (parse-sexpr l) (parse-sexpr r))]
 ;; parse-sexpr : Sexpr -> FLANG
 [(list '- l r) (Minus (parse-sexpr l) (parse-sexpr r))]
 ;; parse-sexpr : Sexpr -> FLANG
 [(list '* l r) (Multipilication (parse-sexpr l) (parse-sexpr r))]
 [(list '/ l r) (Div (parse-sexpr l) (parse-sexpr r))]
 ;; devide sexpr split the numbers to the left and right of the operator
 [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
 ;; call sexpr split the function and the argument
 [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
 ;; equal sexpr split the numbers to the left and right of the operator
 [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
 ;; greater than sexpr , check if the number on the left is bigger than the number on the right
 [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
 ;; smaller than sexpr , check if the number on the left is smaller than the number on the right
 [(list 'not exp) (Not (parse-sexpr exp))]
 ;; not sexpr , check if the number is not equal to 0
 [(cons 'if more)
 ;; if sexpr , check if the condition is true or false and return the corresponding FLANG for example 
  (match sexpr
    [(list 'if con (list 'then-do then) (list 'else-do else))(If (parse-sexpr con) (parse-sexpr then) (parse-sexpr else))]
    [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
;; if the condition is true return the then-do part and if the condition is false return the else-do part
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
 ;; if the syntax is not correct return an error message


(: parse : String -> FLANG)
;; declare parse function
(define (parse code)
;; define parse function
 (parse-sexpr (string->sexpr code))
;; parse-sexpr : Sexpr -> FLANG
)


(: subst : FLANG Symbol FLANG -> FLANG)
;; declare subst function
 (define (subst expr from to)
;; define subst function
 (cases expr
;; cases expr
    [(Num n) expr]
;; if expr is a number return expr
    [(Plus l r) (Plus (subst l from to) (subst r from to))]
;; if expr is a plus return the sum of the left and right of the operator
    [(Minus l r) (Minus (subst l from to) (subst r from to))]
;; if expr is a minus return the difference of the left and right of the operator
    [(Multipilication l r) (Multipilication (subst l from to) (subst r from to))]
;; if expr is a Mult return the product of the left and right of the operator
    [(Div l r) (Div (subst l from to) (subst r from to))]
;; if expr is a division return the quotient of the left and right of the operator
    [(With name named body)(With name (subst named from to)(if (eq? from name) body (subst body from to)))]
;; if expr is a with return the body of the with
    [(Fun name body)(Fun name (if (eq? name from) body (subst  body from to)))]
;; if expr is a function return the body of the function
   [(Call fun-expr arg-expr)(Call (subst fun-expr from to) (subst arg-expr from to))]
;; if expr is a call return the function and the argument
   [(Id name) (if (eq? from name) to expr)]
;; if expr is an id return the name
   [(Bool b) expr]
;; if expr is a boolean return expr
   [(Equal l r) (Equal (subst l from to) (subst r from to))]
;; if expr is an equal return the equality of the left and right of the operator
   [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
;; if expr is a bigger return the bigger of the left and right of the operator
   [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
;; if expr is a smaller return the smaller of the left and right of the operator
   [(Not exp) (Not (subst exp from to))]
;; if expr is a not return the not of the expression
   [(If con then else) (If (subst con from to) (subst then from to) (subst else from to))])) 


(: Num->number : FLANG -> Number)
;; declare Num->number function
(define (Num->number e)
;; define Num->number function
 (cases e
;; cases e
 [(Num n) n]
;; if e is a number return n
 [else (error 'Num->number "expected a number, got: ~s" e)]))
;; if e is not a number return an error message

 (: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; declare arith-op function
 (define (arith-op op expr1 expr2)
;; define arith-op function
 (Num (op (Num->number expr1) (Num->number expr2))))
;; if expr1 and expr2 are numbers return the operation of the operator and the numbers
 (: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
;; declare logic-op function
 (define (logic-op op expr1 expr2)
;; define logic-op function
 (Bool (op (Num->number expr1) (Num->number expr2))))
;; if expr1 and expr2 are numbers return the operation of the operator and the numbers
 (: flang->bool : FLANG -> Boolean)
;; declare flang->bool function
 (define (flang->bool e)
;; define flang->bool function
 (cases e
;; cases e
   [(Bool b) b]
;; if e is a boolean return b
   [else #t]))
;; if e is not a boolean return #t



(: eval : FLANG -> FLANG)
;; evaluates FLANG expressions by reducing them to *expressions*
 (define (eval expr)
 ;; (displayln expr)
 (cases expr
 ;; Original interpreter's code omitted...
   [(Num n) expr]
   ;; if both E1 and E2 evaluate to numbers
   [(Plus l r)  (arith-op + (eval l) (eval r))]
   ;; if plus evaluates to a number and the other evaluates to a boolean
   [(Minus l r) (arith-op - (eval l) (eval r))]
   ;; if minus evaluates to a number and the other evaluates to a boolean
   [(Multipilication l r) (arith-op * (eval l) (eval r))]
   ;; if Mult evaluates to a number and the other evaluates to a boolean
   [(Div l r) (arith-op / (eval l) (eval r))]
   ;; if Div evaluates to a number and the other evaluates to a boolean
   [(With name named body) (eval (subst body name (eval named)))]
   ;; if the name is not defined
   [(Id name) (error 'eval "free identifier: ~s" name)]
   ;; if the name is defined
   [(Fun name body) expr]
   ;; fun is a function expression
   [(Call fun-expr arg-expr) (let ([fval (eval fun-expr)]) (cases fval [(Fun name body) 
   ;; call the function 
   (eval (subst body name (eval arg-expr)))]
   ;; if the function is not defined
    [else (error 'eval "expected a function, got: ~s" fval)]))]
    ;; if the function is defined
  [(Bool b) expr]
  ;; if both E1 and E2 evaluate to numbers
  [(Equal l r) (logic-op = (eval l) (eval r))]
  ;; if plus evaluates to a number and the other evaluates to a boolean
  [(Bigger l r) (logic-op > (eval l) (eval r))]
  ;; if minus evaluates to a number and the other evaluates to a boolean
  [(Smaller l r) (logic-op < (eval l) (eval r))]
  ;; if Mult evaluates to a number and the other evaluates to a boolean
  [(If l m r)
  ;; if the condition is true
   (let ([ival (eval l)])
   ;; if the condition is false
     (if (eq? (flang->bool ival) #t) (eval m) (eval r)))]
    ;; if the condition is not a boolean 
  [(Not exp) (Bool (not (flang->bool (eval exp))))]))


(: run : String -> (U Number Boolean FLANG))
;; evaluate a FLANG program contained in a string
 (define (run str)
 ;; (displayln str)
 (let ([output (eval (parse str))])
 ;; (displayln output)
(cases output
;; if the output is a number
 [(Num n) n]
 ;; if the output is a boolean
 [(Bool b) b]
;; if the output is a FLANG expression
 [else output])))




;; tests
;; tests
(test (run "True") => true)
(test (run "{not True}") => false)
(test (run "{> 3 44}") => false)
(test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4)
(test (run "{with {x 8}
{if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
(test (run "{with {x 0}
{if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
(test (run "{with {c True}
{if c {then-do {> 2 1}} {else-do 2}}}")
=> true)
(test (run "{with {foo {fun {x}
{if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}")
=> (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
(test (run "{with {x 0}
{if {> x 0} {/ 2 x} x}}")
=error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
(test (run "true") =error> "eval: free identifier: true")
(test (run "{< false 5}") =error> "eval: free identifier: false")
(test (run "{< False 5}")
=error> "Num->number: expected a number, got: #(struct:Bool #f)")
(test (run "{not True}") => false)
(test (run "{< 3 44}") => true)
(test (run "{if {= 3 3} {then-do 4} {else-do 5}}") => 4)
(test (run "{with {x 0}
{if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
(test (run "{with {c True}
{if c {then-do {> 2 1}} {else-do {+ 2 2}}}}")
=> true)
(test (run "{with {foo {fun {x}
{if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}")
=> (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
(test (run "{with {x 0}
{if {> x 0} {/ 2 x} x}}")
=error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
(test (run "true") =error> "eval: free identifier: true")
(test (run "{< false 5}") =error> "eval: free identifier: false")
(test (run "{< False 5}")
=error> "Num->number: expected a number, got: #(struct:Bool #f)")
(test (run "{with {x 0}
{if {> x 0} {then-do {/ 2 x}} {else-do {+ 2 x}}}}") => 2)
(test (run "{with {x -3}
{if {> x 0} {then-do {/ x 2}} {else-do {+ 2 x}}}}") => -1)
(test (run "{not False}") => true)
(test (run "{< 3 3}") => false)
(test (run "{if {= 3 4} {then-do 4} {else-do 5}}") => 5)
(test (run "{with {x 8}
{if {< x 0} {then-do {/ 2 x}} {else-do x}}}") => 8)
(test (run "{with {x 0}
{if {< x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
(test (run "{if {> 2 3} {then-do True} {else-do {+ 2 2}}}") => 4)
(test (run "{with {c False}
{if c {then-do {> 2 1}} {else-do {+ 2 2}}}}")
=> 4)
(test (run "{with {x 0}
{if {< x 0} {/ 2 x} x}}")
=error> "parse-sexpr: bad `if' syntax in (if (< x 0) (/ 2 x) x)")
(test (run "false")
=error> "eval: free identifier: false")
(test (run "{> false 5}") =error> "eval: free identifier: false")
(test (run "{> True 5}")
=error> "Num->number: expected a number, got: #(struct:Bool #t)")
(test (run "{with {x 0}
{if {< x 0} {then-do {/ 2 x}} {else-do {- 2 x}}}}") => 2)
(test (run "{with {x 3}
{if {< x 0} {then-do {/ x 2}} {else-do {- 2 x}}}}") => -1)
(test (run "{with {x {+ 3 4}}
{if {< x 0} {then-do {/ x 2}} {else-do {- 2 x}}}}") => -5)


