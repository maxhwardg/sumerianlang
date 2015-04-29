

[<EntryPoint>]
let main argv = 
    Seq.iter (printfn "Stack after evaluation: %A") 
        ( (fun _ -> 
                try
                    printf "#> "
                    let codes, defs = 
                        System.Console.ReadLine() |> Parse.parse |> Compile.compile
                    VM.runVM defs codes
                with
                    | e -> printfn "%A" e; []
                ) |> Seq.initInfinite )
    0 // return an integer exit code
