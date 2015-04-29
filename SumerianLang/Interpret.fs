module Interpret

open Parse

let sameType a b =
    match (a, b) with
    | Int(_), Int(_) -> true
    | Float(_), Float(_) -> true
    | Bool(_), Bool(_) -> true
    | Block(_), Block(_) -> true
    | Id(_), Id(_) -> true
    | _ -> false

type Scope(parent:Scope option) =
    let map = System.Collections.Generic.Dictionary<string, Term list>()
    member this.Find(k:string) =
        if map.ContainsKey(k) then Some(map.[k])
        elif Option.isSome parent then parent.Value.Find(k)
        else None
    member this.Add(k, v) = map.Add(k, v)


module BuiltinFunctions =
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
        | v :: Block(block) :: t -> Block(v :: block) :: t
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
        | Int(_) :: _ as stack -> Id("int") :: stack
        | Float(_) :: _ as stack -> Id("float") :: stack
        | Bool(_) :: _ as stack -> Id("bool") :: stack
        | _ -> failwith "Invalid arguments for function 'type of'"

let (|Keyword|_|) = function
    | "add" -> Some(2, BuiltinFunctions.add)
    | "mult" -> Some(2, BuiltinFunctions.mult)
    | "div" -> Some(2, BuiltinFunctions.div)
    | "sub" -> Some(2, BuiltinFunctions.sub)
    | "mod" -> Some(2, BuiltinFunctions.modulus)
    | "cons" -> Some(2, BuiltinFunctions.cons)
    | "decons" -> Some(2, BuiltinFunctions.decons)
    | "eq" -> Some(2, BuiltinFunctions.eq)
    | "lt" -> Some(2, BuiltinFunctions.lt)
    | "gt" -> Some(2, BuiltinFunctions.gt)
    | "toInt" -> Some(2, BuiltinFunctions.toInt)
    | "toFloat" -> Some(2, BuiltinFunctions.toFloat)
    | "and" -> Some(2, BuiltinFunctions.logicalAnd)
    | "or" -> Some(2, BuiltinFunctions.logicalOr)
    | "not" -> Some(2, BuiltinFunctions.not)
    | "AND" -> Some(2, BuiltinFunctions.bitwiseAnd)
    | "XOR" -> Some(2, BuiltinFunctions.xor)
    | "OR" -> Some(2, BuiltinFunctions.bitwiseOr)
    | "type" -> Some(1, BuiltinFunctions.typeOf)
    | _ -> None


let defineSymbol (env:Scope) = function
    | Id(id) :: Block(block) :: tail ->
        if env.Find id |> Option.isSome then failwith (sprintf "Id %s already defined" id)
        else env.Add(id, block)
        tail
    | _ -> failwith "Invalid arguments for function 'define'"

let rec forceBlock stack (env:Scope) = List.fold (fun s t -> force env (t::s)) stack
and force (env:Scope) = function
    | Id("if") :: stack ->
        match (forcen 3 env stack) with
        | Block(thenb) :: Block(elseb) :: Bool(cond) :: rest ->
            if cond then forceBlock thenb env rest else forceBlock elseb env rest
        | _ -> failwith "Invalid arguments for function 'if'"
    | Id("define") :: stack -> forcen 2 env stack |> defineSymbol env
    | Id(id) :: stack ->
        match id with
        | Keyword(args, fn) -> forcen args env stack |> fn
        | _ ->
            match env.Find(id) with
            | Some(terms) -> forceBlock stack (Scope(Some(env))) terms
            | None -> Id(id) :: stack
    | anything -> anything
and forcen n (env:Scope) stack = 
    let rec reversedn acc n = function
        | rem when n = 0 -> acc, rem
        | h :: t -> reversedn (h::acc) (n-1) t
        | _ -> failwith "Stack undeflow"
    let toForce, remainder = reversedn [] n stack
    forceBlock remainder env toForce



let interpret stream = force (Scope(None)) stream // |> List.head