open System

// For more information see https://aka.ms/fsharp-console-apps

let args = Environment.GetCommandLineArgs()

let runFile arg =
    printfn "Welcome to runFile %s" arg

let runPrompt () =
    printfn "Welcome to runPrompt"
    let input = Console.ReadLine()
    printfn "Your input was: %s" input


if args.Length > 2 then
    printf "Usage: flox [script]"
    exit 64
elif args.Length = 2 then
    runFile args.[1]
else
    runPrompt ()

exit 0

