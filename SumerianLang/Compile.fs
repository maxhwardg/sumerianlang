module Compile

let mergeMaps m m' = 
    Map.fold (fun state key value -> 
        if Map.containsKey key state |> not then Map.add key value state else state) m m'


/// Lift all definitons from the parse term stream into a map
let liftDefinitions = 
    let rec lifer path defs filtered = function
        | Parse.Definition(name, body) :: tail ->
            if Map.containsKey (name::path) defs then
                failwith "Redefinition!"
            else
                let interiorDefs, liftedBody = lifer (name::path) Map.empty [] body
                lifer 
                    path 
                    (mergeMaps (Map.add (name::path) liftedBody defs) interiorDefs)
                    filtered 
                    tail
        | [] -> defs, List.rev filtered
        | head :: tail -> lifer path defs (head::filtered) tail
    lifer [] Map.empty []


let rec toOpCode path defs = function
    | Parse.Id(id) when VM.Keywords.keywords.ContainsKey id -> 
        VM.Keyword(VM.Keywords.keywords.Item id)
    | Parse.Id(id) -> 
        if Map.containsKey (id::path) defs then
            VM.Call(Map.find (id::path) defs)
        elif Map.containsKey (id::(List.tail path)) defs then
            VM.Call(Map.find (id::(List.tail path)) defs) // recursion
        else
            failwith (sprintf "Variable '%A' undefined" id)
    | Parse.Bool(b) -> VM.Bool(b)
    | Parse.Int(i) -> VM.Int(i)
    | Parse.Float(f) -> VM.Float(f)
    | Parse.Block(b) -> VM.Block(List.map (toOpCode path defs) b)
    | Parse.Definition(_, _) -> failwith "Invalid definition"

let compileLayer path defs = List.map (toOpCode path defs)

let compile stream =
    let defs, rem = liftDefinitions stream
    let pathToCode = 
        Seq.zip (seq { for k, _ in Map.toSeq defs -> k }) (Seq.initInfinite id)
        |> Map.ofSeq
    let codeToValue = 
        [| for k, v in Map.toSeq defs -> compileLayer k pathToCode v |]
    compileLayer [] pathToCode rem, codeToValue
