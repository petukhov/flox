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

let keywordToToken = function
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

    let addToken (tokenType: TokenType) literal =
        let text = source.[start..current - 1]
        let newToken = {
            tokenType = tokenType;
            lexeme = text;
            literal = literal;
            line = line }
        tokens <- newToken :: tokens

    let advance () =
        let res = source.[current]
        current <- current + 1
        res

    let matchChar expected =
        if not (isAtEnd ()) && source.[current] = expected
        then
            current <- current + 1
            true
        else false

    let peek () =
        if isAtEnd () then END_FILE_CHAR else source.[current]

    let advanceToLineEnd () =
        while peek () <> '\n' && not (isAtEnd ()) do
            advance () |> ignore

    let string () =
        while peek () <> '\"' && not (isAtEnd ()) do
            if peek () = '\n' then line <- line + 1
            advance () |> ignore

        if isAtEnd ()
            then
                State.hadError <- true
                printf "Unterminated string at line %d." line
            else
                advance () |> ignore
                let value = source.[start + 1..current - 2]
                addToken STRING value

    let peekNext () =
        if current + 1 >= source.Length
        then END_FILE_CHAR
        else source.[current + 1]

    let number () =
        while isDigit (peek ()) do advance () |> ignore

        if peek() = '.' && isDigit (peekNext ()) then
            advance () |> ignore
            while isDigit (peek ()) do advance () |> ignore

        addToken NUMBER (Double.Parse source.[start..current - 1])

    let identifier () =
        while isAlphaNumeric (peek ()) do advance () |> ignore;
        let text = source.[start .. current - 1]
        addToken (keywordToToken text) None
    
    let scanToken () =
        let c = advance ()
        match c with
        | '(' -> addToken LEFT_PAREN None
        | ')' -> addToken RIGHT_PAREN None
        | '{' -> addToken LEFT_BRACE None
        | '}' -> addToken RIGHT_BRACE None
        | ',' -> addToken COMMA None
        | '.' -> addToken DOT None
        | '-' -> addToken MINUS None
        | '+' -> addToken PLUS None
        | ';' -> addToken SEMICOLON None
        | '*' -> addToken STAR None
        | '!' -> addToken (if matchChar '=' then BANG_EQUAL else BANG) None
        | '=' -> addToken (if matchChar '=' then EQUAL_EQUAL else EQUAL) None
        | '<' -> addToken (if matchChar '=' then LESS_EQUAL else LESS) None
        | '>' -> addToken (if matchChar '=' then GREATER_EQUAL else GREATER) None
        | '/' -> if matchChar '/' then advanceToLineEnd () else addToken SLASH None
        | ' ' | '\r' | '\t' -> ()
        | '\n' -> line <- line + 1
        | '\"' -> string ()
        | _   ->
            if isDigit c then number ()
            elif isAlpha c then identifier ()
            else
                // TODO: Add proper printing to console here.
                printfn "Unexpected error at line %d." line
                State.hadError <- true

    while not (isAtEnd ()) do
        start <- current
        scanToken ()
    
    List.rev tokens
    