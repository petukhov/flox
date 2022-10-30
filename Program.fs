module flox.Main

open System

let readLines (filePath:string) = seq {
    use sr = new IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let report line where message =
    printf "[line %i] Error %s: %s" line where message
    State.hadError <- true

let error line message =
    report line "" message

let scanTokens source =
    ["hello"; "world"]

let run source =
    let tokens = scanTokens source
    for t in tokens do
        printfn "%s" t

let runFile pathStr =
    printfn "Reading source file: %s" pathStr
    let allText = readLines pathStr |> String.concat "\n"
    //printfn "%s" allText
    run allText
    if State.hadError = true then
        exit 65

let runPrompt () =
    printfn "Welcome to interactive mode!"
    let rec loop input =
        match input with
        | "" -> ()
        | _ -> 
            //printfn "Your input was: %s" input
            run input
            State.hadError <- false
            printf "> "
            loop (Console.ReadLine ())
    printf "> "
    loop (Console.ReadLine ())

//////////////////////////////////// Execution

let args = Environment.GetCommandLineArgs ()

if args.Length > 2 then
    printf "Usage: flox [script]"
    exit 64
elif args.Length = 2 then
    runFile args.[1]
else
    runPrompt ()

exit 0

