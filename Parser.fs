module internal flox.Parser

type Expr = 
    | Binary of left:Expr * operator:Token * right:Expr
    | Grouping of expression:Expr
    | Literal of value:obj
    | Unary of operator:Token * right:Expr

let BANG_EQUAL = "!="
let EQUAL_EQUAL = "=="

let matchToken = ()

let equality () = 
    let mutable expr = comparison ()
    while matchToken BANG_EQUAL EQUAL_EQUAL do
        let operator = previous ()
        let right = comparison ()
        expr = Binary(expr, operator, right)
    expr

let expression () = equality ()
