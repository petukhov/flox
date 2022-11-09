module internal flox.Expr

open flox.Lexing

//// This is coming from the book:

// "Binary   : Expr left, Token operator, Expr right",
// "Grouping : Expr expression",
// "Literal  : Object value",
// "Unary    : Token operator, Expr right"


// Should be converted to this:

type Expr =
    | Binary of left : Expr * operator : Token * right : Expr
    | Grouping of expression : Expr
    | Literal of value : obj option
    | Unary of operator: Token * right : Expr


let internal createToken tokenType text literal line =
    {   tokenType = tokenType
        lexeme = text
        literal = literal
        line = line   }

let example = 
    Binary(
        Unary(createToken MINUS "-" None 1, Literal(Some(123 :> obj))),
        createToken STAR "*" None 1,
        Grouping(Literal(Some(45.67 :> obj)))
    )

let rec prettyPrint = function
    | Binary(left, operator, right) -> parenthesize operator.lexeme [left; right]
    | Grouping(expression) -> parenthesize "group" [expression]
    | Literal(value) -> match value with Some(v) -> string v | None -> "nil"
    | Unary(operator, right) -> parenthesize operator.lexeme [right]

and parenthesize name exprs =
    let paramsStr = exprs |> List.map prettyPrint |> String.concat " "
    $"({name} {paramsStr})"