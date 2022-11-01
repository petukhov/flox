module internal flox.Lexing

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
    //override this.ToString() =
    //    string this.tokenType + " " + this.lexeme + " " + string this.literal

let andEnsure mapping option =
    match option with
    | true -> mapping
    | false -> false

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

    let advance() =
        let res = source.[current]
        current <- current + 1
        res

    let matchChar expected =
        if not (isAtEnd ()) && source.[current] = expected
        then
            current <- current + 1
            true
        else false

    let scanToken () =
        let c = advance()
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
        | _   ->
            // add proper printing to console here
            printfn "Unexpected error at %d" line
            State.hadError <- true

    //printfn "length is %d" (String.length source)
    while not (isAtEnd ()) do
        start <- current
        scanToken ()
    
    List.rev tokens
    