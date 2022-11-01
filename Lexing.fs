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

//type LexerContext =
//    { source: string
//      start: int
//      current: int
//      line: int  }    

let scanTokens (source: string) =
    let mutable start = 0
    let mutable current = 0
    let mutable line = 0
    let mutable tokens = []

    let addToken (tokenType: TokenType) literal =
        printfn "addToken start: %d current: %d" start current
        let text = source.[start..current - 1]
        let newToken = {
            tokenType = tokenType;
            lexeme = text;
            literal = literal;
            line = line }
        tokens <- newToken :: tokens

    let advance() =
        //printf "%d" current
        let res = (source |> Array.ofSeq).[current]
        current <- current + 1
        res

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
        | _   ->
            // add proper printing to console here
            printfn "Unexpected error at %d" line
            State.hadError <- true

    //printfn "length is %d" (String.length source)
    while current < String.length source do
        start <- current
        scanToken ()
    
    List.rev tokens
    