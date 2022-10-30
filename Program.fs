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

let runFile pathStr =
    printfn "Reading file: %s" pathStr
    let allText = readLines pathStr |> String.concat "\n"
    printfn "%s" allText

let runPrompt () =
    printfn "Welcome to runPrompt"
    let rec loop input =
        match input with
        | "" -> ()
        | _ -> 
            printfn "Your input was: %s" input
            loop (Console.ReadLine())
    loop (Console.ReadLine())

//////////////////////////////////// Execution

let args = Environment.GetCommandLineArgs()

if args.Length > 2 then
    printf "Usage: flox [script]"
    exit 64
elif args.Length = 2 then
    runFile args.[1]
else
    runPrompt ()

exit 0

