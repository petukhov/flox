module internal flox.Expr

open flox.Lexing

//// This is coming from the book:

// "Binary   : Expr left, Token operator, Expr right",
// "Grouping : Expr expression",
// "Literal  : Object value",
// "Unary    : Token operator, Expr right"

// Should be automatically converted to this:

type Expr =
    | Binary of left : Expr * operator : Token * right : Expr
    | Grouping of expression : Expr
    | Literal of value : obj
    | Unary of operator: Token * right : Expr


let internal createToken tokenType text literal line =
    {
        tokenType = tokenType;
        lexeme = text;
        literal = literal;
        line = line
    }

let internal example = 
    Binary(
        Unary(createToken MINUS "-" None 1, Literal(123)), 
        createToken STAR "*" None 1, Grouping(Literal(45.67))
    )