// TODO:
// 1. pass errors as a return value from lexer
// 2. add unit testing
// 3. add auto-formatting and linting

module flox.Main

open System
open Expr

let readLines (filePath: string) =
    seq {
        use sr = new IO.StreamReader(filePath)

        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let report line where message =
    printf "[line %i] Error %s: %s" line where message
    State.hadError <- true

let error line message = report line "" message

let run source =
    let tokens = Lexing.scanTokens source

    for t in tokens.Value do
        printfn "%A" t

    let ast = Parser.parse tokens

    match ast with
    | Some ast -> printfn "AST: %s" (prettyPrint ast)
    | None -> printfn "Error running the source code."
// printfn "AST: %A" ast
// for t in tokens do
//     printfn "%A" t

let runFile pathStr =
    printfn "Reading source file: %s" pathStr
    let allText = readLines pathStr |> String.concat "\n"
    run allText

    if State.hadError = true then
        exit 65

let runPrompt () =
    printfn "Welcome to interactive mode!"

    let rec loop input =
        match input with
        | "" -> ()
        | _ ->
            run input
            State.hadError <- false
            printf "> "
            loop (Console.ReadLine())

    printf "> "
    loop (Console.ReadLine())

///////////////// Execution ///////////////////

let args = Environment.GetCommandLineArgs()

if args.Length > 2 then
    printfn "Usage: flox [script]"
    exit 64
elif args.Length = 2 then
    runFile args.[1]
else
    runPrompt ()

exit 0
