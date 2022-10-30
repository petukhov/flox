open System

// For more information see https://aka.ms/fsharp-console-apps

let readLines (filePath:string) = seq {
    use sr = new IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let args = Environment.GetCommandLineArgs()

let runFile pathStr =
    printfn "Reading file: %s" pathStr
    let allText = readLines pathStr |> String.concat "\n"
    printfn "%s" allText

let runPrompt () =
    printfn "Welcome to runPrompt"
    let rec processInput input =
        match input with
        | "" -> ()
        | _ -> 
            printfn "Your input was: %s" input
            processInput (Console.ReadLine())
    processInput (Console.ReadLine())


if args.Length > 2 then
    printf "Usage: flox [script]"
    exit 64
elif args.Length = 2 then
    runFile args.[1]
else
    runPrompt ()

exit 0

