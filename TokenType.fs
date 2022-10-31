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
    override this.ToString() =
        string this.tokenType + " " + this.lexeme + " " + string this.literal

type LexerContext =
    { source: string
      start: int
      current: int
      line: int  }

let addToken (tokenType: TokenType) literal =
    //let text = source.substring(start, current)
    literal
    // tokens.add(new Token(type, text, literal, line))

//private void addToken(TokenType type) {
//    addToken(type, null);
//  }

let scanToken () = ()

let scanTokens (source: string) =
    let mutable start = 0
    let mutable current = 0
    let mutable line = 0
    let tokens = [|1..10|]

    while current < String.length source do
        start <- current
        scanToken ()
    
    tokens
    //while (!isAtEnd()) {
    //     // We are at the beginning of the next lexeme.
    //     start = current;
    //     scanToken();
    //   }

    //   tokens.add(new Token(EOF, "", null, line));
    //   return tokens;
    