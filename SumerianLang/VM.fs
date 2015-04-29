module VM

type OpCode = 
    | Int of int
    | Float of float
    | Bool of bool
    | Block of OpCode list
    | Call of int
    | Keyword of int
    | Force


// predefined functions
module Keywords =
    let private sameType a b =
        match (a, b) with
        | Int(_), Int(_) -> true
        | Float(_), Float(_) -> true
        | Bool(_), Bool(_) -> true
        | Block(_), Block(_) -> true
        | _ -> false
    let add = function
        | Int(av) :: Int(bv) :: t -> Int(av + bv) :: t
        | Float(av) :: Float(bv) :: t -> Float(av + bv) :: t
        | Block(av) :: Block(bv) :: t -> Block(bv @ av) :: t
        | _ -> failwith "Invalid arguments to function '+'"
    let mult = function
        | Int(av) :: Int(bv) :: t -> Int(av * bv) :: t
        | Float(av) :: Float(bv) :: t -> Float(av * bv) :: t
        | _ -> failwith "Invalid arguments to function '*'"
    let div = function
        | Int(av) :: Int(bv) :: t -> Int(bv / av) :: t
        | Float(av) :: Float(bv) :: t -> Float(bv / av) :: t
        | _ -> failwith "Invalid arguments to function '/'"
    let sub = function
        | Int(av) :: Int(bv) :: t -> Int(bv - av) :: t
        | Float(av) :: Float(bv) :: t -> Float(bv - av) :: t
        | _ -> failwith "Invalid arguments to function '-'"
    let modulus = function
        | Int(av) :: Int(bv) :: t -> Int(bv % av) :: t
        | _ -> failwith "Invalid arguments to function '%'"
    let cons = function
        | Block(block) :: v :: t -> Block(v :: block) :: t
        | _ -> failwith "Invalid arguments for function 'cons'"
    let decons = function
        | Block(h :: t) :: rest -> h :: Block(t) :: rest
        | _ -> failwith "Invalid arguments for function 'decons'"
    let eq = function
        | a :: b :: t when sameType a b -> Bool(a = b) :: t
        | _ -> failwith "Invalid arguments for function '='"
    let not = function
        | Bool(v) :: t -> Bool(not v) :: t
        | _ -> failwith "Invalid arguments for function 'not'"
    let lt = function
        | Int(av) :: Int(bv) :: t -> Bool(bv < av) :: t
        | Float(av) :: Float(bv) :: t -> Bool(bv < av) :: t
        | _ -> failwith "Invalid arguments for function '<'"
    let gt = function
        | Int(av) :: Int(bv) :: t -> Bool(bv > av) :: t
        | Float(av) :: Float(bv) :: t -> Bool(bv > av) :: t
        | _ -> failwith "Invalid arguments for function '>'"
    let logicalAnd = function
        | Bool(av) :: Bool(bv) :: t -> Bool(bv && av) :: t
        | _ -> failwith "Invalid arguments for function 'and'"
    let logicalOr = function
        | Bool(av) :: Bool(bv) :: t -> Bool(bv || av) :: t
        | _ -> failwith "Invalid arguments for function 'or'"
    let xor = function
        | Int(av) :: Int(bv) :: t -> Int(bv ^^^ av) :: t
        | _ -> failwith "Invalid arguments for function 'xor'"
    let bitwiseAnd = function
        | Int(av) :: Int(bv) :: t -> Int(bv &&& av) :: t
        | _ -> failwith "Invalid arguments for function 'bitwise and'"
    let bitwiseOr = function
        | Int(av) :: Int(bv) :: t -> Int(bv ||| av) :: t
        | _ -> failwith "Invalid arguments for function 'bitwise or'"
    let toInt = function
        | Float(v) :: t -> Int(int v) :: t
        | _ -> failwith "Invalid arguments for function 'to int'"
    let toFloat = function
        | Int(v) :: t -> Float(float v) :: t
        | _ -> failwith "Invalid arguments for function 'to float'"
    let typeOf = function
        | Int(_) :: _ as stack -> Int(0) :: stack
        | Float(_) :: _ as stack -> Int(1) :: stack
        | Bool(_) :: _ as stack -> Int(2) :: stack
        | _ -> failwith "Invalid arguments for function 'type of'"
    let dup = function
        | v :: t -> v :: v :: t
        | _ -> failwith "Invalid arguments for function 'dup'"
    let swap = function
        | a :: b :: t -> b :: a :: t
        | _ -> failwith "Invalid arguments for function 'swap'"

    let explode = function
        | Block(b) :: t -> (List.rev b) @ t
        | _ -> failwith "Invalid arguments for function 'explode'"

    let dip = function
         | Block(b) :: c :: t -> [c] @ (List.rev b) @ t
         | _ -> failwith "Invalid arguments for function 'dip'"

    let branch = function
        | Block(thenb) :: Block(elseb) :: Bool(cond) :: t ->
            List.rev (if cond then thenb else elseb) @ t
        | _ -> failwith "Invalid arguments for function 'if'"

    let pop = function
         | _ :: t -> t
         | _ -> failwith "Invalid arguments for function 'pop'"

    let print = function
        | h :: t -> printfn "%A" h; t
        | _ -> failwith "Invalid arguments for function 'print'"

    let keywords = 
        Seq.zip
            [
                "add"; "mult"; "div"; "sub"; "mod";
                "cons"; "decons"; "eq" ; "lt"; "gt";
                "toInt"; "toFloat"; "and"; "or"; "not";
                "AND"; "XOR"; "OR"; "type"; "dup";
                "swap"; "dip"; "explode"; "if"; "pop";
                "print"
            ]
            (Seq.initInfinite id)
        |> Map.ofSeq

    let functions = 
        [|
            (2, add); (2, mult); (2, div); (2, sub); 
            (2, modulus); (2, cons); (2, decons); (2, eq); 
            (2, lt); (2, gt); (2, toInt); (2, toFloat);
            (2, logicalAnd); (2, logicalOr); (2, not);
            (2, bitwiseAnd); (2, xor); (2, bitwiseOr);
            (1, typeOf); (1, dup); (2, swap); (2, dip);
            (1, explode); (3, branch); (1, pop); (1, print)
        |]

let printId = Keywords.keywords.Item "print"

let rec force (definitions:OpCode list[]) = function
    | Call(id) :: stack -> force definitions (List.rev definitions.[id] @ stack)
    | Keyword(id) :: stack -> 
        let args, func = Keywords.functions.[id]
        let rec forceFeed n = function
            | stack when n <= 0 -> stack
            | Call(_) :: _ as stack -> forceFeed n (force definitions stack)
            | Keyword(_) :: _  as stack -> forceFeed n (force definitions stack)
            | h :: t -> h :: (forceFeed (n-1) t)
            | [] -> []
        forceFeed args stack
        |> func
    | v -> v // primitive values need not be forced


let vmExecutionStep definitons stack = function
    | Force -> force definitons stack // evaluate thunk at top of stack
    | Keyword(id) as print when id = printId -> force definitons (print::stack)
    | func -> func :: stack // lazy, everything is a function/thunk

let runVM definitions = List.fold (fun state elem -> vmExecutionStep definitions state elem) []
    