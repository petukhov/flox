module internal flox.Lexing

open System

let END_FILE_CHAR = '\r'

type TokenType =
    // Single-character tokens.
    |LEFT_PAREN |RIGHT_PAREN |LEFT_BRACE |RIGHT_BRACE
    |COMMA |DOT |MINUS |PLUS |SEMICOLON |SLASH |STAR

    // One or two character tokens.
    |BANG |BANG_EQUAL
    |EQUAL |EQUAL_EQUAL
    |GREATER |GREATER_EQUAL
    |LESS |LESS_EQUAL

    // Literals.
    |IDENTIFIER |STRING |NUMBER

    // Keywords.
    |AND |CLASS |ELSE |FALSE |FUN |FOR |IF |NIL |OR
    |PRINT |RETURN |SUPER |THIS |TRUE |VAR |WHILE

    | EOF

type Token = 
    { tokenType: TokenType
      lexeme: string
      literal: obj
      line: int }

let keywordToTokenType = function
    | "and"    -> AND
    | "class"  -> CLASS
    | "else"   -> ELSE
    | "false"  -> FALSE
    | "for"    -> FOR
    | "fun"    -> FUN
    | "if"     -> IF
    | "nil"    -> NIL
    | "or"     -> OR
    | "print"  -> PRINT
    | "return" -> RETURN
    | "super"  -> SUPER
    | "this"   -> THIS
    | "true"   -> TRUE
    | "var"    -> VAR
    | "while"  -> WHILE
    | _        -> IDENTIFIER

let isDigit c = c >= '0' && c <= '9'

let isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let isAlphaNumeric c = isAlpha c || isDigit c

let scanTokens (source: string) =
    let mutable start = 0
    let mutable current = 0
    let mutable line = 0
    let mutable tokens = []

    let isAtEnd () = current >= String.length source

    let addToken_ (tokenType: TokenType) literal =
        let text = source.[start..current - 1]
        let newToken = {
            tokenType = tokenType;
            lexeme = text;
            literal = literal;
            line = line }
        tokens <- newToken :: tokens

    let advance_ () =
        let res = source.[current]
        current <- current + 1
        res

    let matchChar_ expected =
        if not (isAtEnd ()) && source.[current] = expected
        then
            current <- current + 1
            true
        else false

    let peek () =
        if isAtEnd () then END_FILE_CHAR else source.[current]

    let advanceToLineEnd_ () =
        while peek () <> '\n' && not (isAtEnd ()) do
            advance_ () |> ignore

    let string_ () =
        while peek () <> '\"' && not (isAtEnd ()) do
            if peek () = '\n' then line <- line + 1
            advance_ () |> ignore

        if isAtEnd ()
            then
                State.hadError <- true
                printf "Unterminated string at line %d." line
            else
                advance_ () |> ignore
                let value = source.[start + 1..current - 2]
                addToken_ STRING value

    let peekNext () =
        if current + 1 >= source.Length
        then END_FILE_CHAR
        else source.[current + 1]

    let number_ () =
        while isDigit (peek ()) do advance_ () |> ignore

        if peek() = '.' && isDigit (peekNext ()) then
            advance_ () |> ignore
            while isDigit (peek ()) do advance_ () |> ignore

        addToken_ NUMBER (Double.Parse source.[start..current - 1])

    let identifier_ () =
        while isAlphaNumeric (peek ()) do advance_ () |> ignore;
        let text = source.[start .. current - 1]
        addToken_ (keywordToTokenType text) None
    
    let scanToken_ () =
        let c = advance_ ()
        match c with
        | '(' -> addToken_ LEFT_PAREN None
        | ')' -> addToken_ RIGHT_PAREN None
        | '{' -> addToken_ LEFT_BRACE None
        | '}' -> addToken_ RIGHT_BRACE None
        | ',' -> addToken_ COMMA None
        | '.' -> addToken_ DOT None
        | '-' -> addToken_ MINUS None
        | '+' -> addToken_ PLUS None
        | ';' -> addToken_ SEMICOLON None
        | '*' -> addToken_ STAR None
        | '!' -> addToken_ (if matchChar_ '=' then BANG_EQUAL else BANG) None
        | '=' -> addToken_ (if matchChar_ '=' then EQUAL_EQUAL else EQUAL) None
        | '<' -> addToken_ (if matchChar_ '=' then LESS_EQUAL else LESS) None
        | '>' -> addToken_ (if matchChar_ '=' then GREATER_EQUAL else GREATER) None
        | '/' -> if matchChar_ '/' then advanceToLineEnd_ () else addToken_ SLASH None
        | ' ' | '\r' | '\t' -> ()
        | '\n' -> line <- line + 1
        | '\"' -> string_ ()
        | _   ->
            if isDigit c then number_ ()
            elif isAlpha c then identifier_ ()
            else
                // TODO: Add proper printing to console here.
                printfn "Unexpected error at line %d." line
                State.hadError <- true

    while not (isAtEnd ()) do
        start <- current
        scanToken_ ()
    
    List.rev tokens
    