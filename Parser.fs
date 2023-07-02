module flox.Parser

type Expr =
    | Binary of left: Expr * operator: Lexing.Token * right: Expr
    | Grouping of expression: Expr
    | Literal of value: obj option
    | Unary of operator: Lexing.Token * right: Expr

let mutable current = 0
let mutable tokens: Lexing.Token list = []

let peek () = tokens.[current]

let isAtEnd () =
    (peek ()).tokenType = Lexing.TokenType.EOF

let previous () = tokens.[current - 1]

let check (tokenType: Lexing.TokenType) =
    if isAtEnd () then
        false
    else
        printfn "check: %A" tokenType
        (peek ()).tokenType = tokenType

let advance () =
    if not (isAtEnd ()) then
        current <- current + 1

    previous ()

let rec matchToken (types: Lexing.TokenType list) =
    match types with
    | x :: xs ->
        if check x then
            advance () |> ignore
            true
        else
            matchToken xs
    | _ -> false

let consume (tokenType: Lexing.TokenType) (message: string) =
    if check tokenType then
        advance ()
    else
        raise (Error.compileError (peek ()) message)

// this is not used yet
let synchronize () =
    advance () |> ignore

    while not (isAtEnd ()) do
        if (previous ()).tokenType = Lexing.TokenType.SEMICOLON then
            ()
        else
            match (peek ()).tokenType with
            | Lexing.TokenType.CLASS
            | Lexing.TokenType.FUN
            | Lexing.TokenType.VAR
            | Lexing.TokenType.FOR
            | Lexing.TokenType.IF
            | Lexing.TokenType.WHILE
            | Lexing.TokenType.PRINT
            | Lexing.TokenType.RETURN -> ()
            | _ -> advance () |> ignore
    false

let rec primary () =
    if matchToken [ Lexing.TokenType.FALSE ] then
        Literal(Some false)
    elif matchToken [ Lexing.TokenType.TRUE ] then
        Literal(Some true)
    elif matchToken [ Lexing.TokenType.NIL ] then
        Literal(None)
    elif matchToken [ Lexing.TokenType.NUMBER; Lexing.TokenType.STRING ] then
        Literal(Some (previous ()).literal)
    elif matchToken [ Lexing.TokenType.LEFT_PAREN ] then
        let expr = expression ()
        consume Lexing.TokenType.RIGHT_PAREN "Expect ')' after expression." |> ignore
        Grouping(expr)
    else
        raise (Error.compileError (peek ()) "Expect expression.")

and unary () =
    if matchToken [ Lexing.TokenType.BANG; Lexing.TokenType.MINUS ] then
        let operator = previous ()
        let right = unary ()
        Unary(operator, right)
    else
        primary ()

and factor () =
    let mutable expr = unary ()

    while matchToken [ Lexing.TokenType.SLASH; Lexing.TokenType.STAR ] do
        let operator = previous ()
        let right = unary ()
        expr <- Binary(expr, operator, right)

    expr

and term () =
    let mutable expr = factor ()

    while matchToken [ Lexing.TokenType.MINUS; Lexing.TokenType.PLUS ] do
        let operator = previous ()
        let right = factor ()
        expr <- Binary(expr, operator, right)

    expr

and comparison () =
    let mutable expr = term ()

    while matchToken
              [ Lexing.TokenType.GREATER
                Lexing.TokenType.GREATER_EQUAL
                Lexing.TokenType.LESS
                Lexing.TokenType.LESS_EQUAL ] do
        let operator = previous ()
        let right = term ()
        expr <- Binary(expr, operator, right)

    expr

and equality () =
    let mutable expr = comparison ()

    while matchToken [ Lexing.TokenType.BANG_EQUAL; Lexing.TokenType.EQUAL_EQUAL ] do
        let operator = previous ()
        let right = comparison ()
        expr <- Binary(expr, operator, right)

    expr

and expression () = equality ()

let parse (_tokens: Lexing.Token list option) =
    match _tokens with
    | None -> None
    | Some values ->
        tokens <- values
        current <- 0
        try
            Some(expression ())
        with Error.CompileError ->
            None
