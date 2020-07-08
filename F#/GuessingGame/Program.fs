// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    Console.Write("What's your name? ")
    let name = Console.ReadLine()
    Console.Write("Hello, {0}!!", name)
    0 // return an integer exit code
