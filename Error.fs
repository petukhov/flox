module flox.Error

open flox.Lexing

exception CompileError

let report (line : int) (where : string) (message : string) =
    printfn "[line %d] Error%s: %s" line where message

let tokenError (token : Token) (message : string) =
    if token.tokenType = TokenType.EOF
    then report token.line " at end" message
    else report token.line (sprintf " at '%s'"  token.lexeme) message

let compileError (token : Token) (message : string) =
    tokenError token message 
    CompileError