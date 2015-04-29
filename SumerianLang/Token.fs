module Token

type Token =
    {
        node : AST.Node;
        index : int;
    }

let (|Digit|_|) d = if d >= '0' && d <= '9' then Some(int d - int '0') else None

let isDigit = function
    | Digit(_) -> true
    | _ -> false

let (|Bracket|_|) b = 
    match b with
    | '(' -> Some(AST.Paren)
    | '{' -> Some(AST.Curly)
    | '[' -> Some(AST.Square)
    | _ -> None


    (*
/// Operators are just normal Id tokens.
/// However, they are tokenized specially, because they are standalone.
/// Say that 'hello' is an operator.
/// The stream '1hello1' would be tokenized as [1, hello, 1]. Not as [1hello1].
let (|Operator|_|) o = 
    match o with
        | '>' :: '=' :: t -> Some(AST.Id(">="), t)
        | '<' :: '=' :: t -> Some(AST.Id("<="), t)
        | '!' :: '=' :: t -> Some(AST.Id("!="), t)
        | '=' :: '=' :: t -> Some(AST.Id("="), t)
        | '+' :: t -> Some(AST.Id("+"), t)
        | '-' :: t -> Some(AST.Id("-"), t)
        | '*' :: t -> Some(AST.Id("*"), t)
        | '/' :: t -> Some(AST.Id("/"), t)
        | '%' :: t -> Some(AST.Id("%"), t)
        | '!' :: t -> Some(AST.Id("!"), t)
        | '^' :: t -> Some(AST.Id("^"), t)
        | '=' :: t -> Some(AST.Id("="), t)
        | '<' :: t -> Some(AST.Id("<"), t)
        | '>' :: t -> Some(AST.Id(">"), t)
        | _ -> None
        *)

let (|WhiteSpace|_|) c = if System.Char.IsWhiteSpace c then Some(c) else None

let (|Ident|_|) i = 
    match i with
    | WhiteSpace(w) -> None
    | '.' -> None
    | anything -> Some(anything)

/// Converts a stream of characters into a list of tokens
let rec tokenize (stream:char seq) = 
    let rec parseIdent stream i = 
        let rec helper acc = function
            | Ident(i) :: t -> helper (i :: acc) t
            | left -> (AST.Id(new string(acc |> List.rev |> List.toArray)), left)
        helper [i] stream
    let rec parseWs = function
        | WhiteSpace(_) :: t -> parseWs t
        | next -> next
    let rec parseChar = function
        | c :: '\'' :: t -> (AST.Char(c), t)
        | next -> (AST.Error("Invalid character, no closing quotation"), next)
    let parseString stream = 
        let buff = System.Text.StringBuilder()
        let rec helper = function
            | '\\' :: '"' :: t -> buff.Append('"') |> ignore; helper t
            | '"' :: t -> (AST.String(buff.ToString()), t)
            | c :: t -> buff.Append(c) |> ignore; helper t
            | next -> (AST.Error("Invalid string, no closing quotation"), next)
        helper stream
    let parseBrackets brType stream = 
        let openBr = AST.Bracket.openOf brType
        let closeBr = AST.Bracket.closeOf brType
        let rec helper acc counter = function
            | h :: t when counter = 0 && h = closeBr -> (AST.Block (brType, tokenize (List.rev acc) ), t)
            | h :: t when h = closeBr -> helper (closeBr :: acc) (counter-1) t
            | h :: t when h = openBr -> helper (openBr :: acc) (counter+1) t
            | h :: t -> helper (h :: acc) counter t
            | [] -> AST.Error(sprintf "Bracket mismatch, expect closing bracket of type %c" closeBr), []
        helper [] 0 stream
    let parseNumber stream num = 
        let rec floatHelper num mult = function
            | Digit(d) :: t -> floatHelper (num + double d*mult) (mult/10.0) t
            | left -> (AST.Float(num), left)
        let rec intHelper num = function
            | Digit(d) :: t -> intHelper (num*10 + d) t
            | '.' :: d :: t when isDigit d -> floatHelper (double num) 0.1 (d :: t)
            | '.' :: _ as left -> AST.Error("Invalid decimal point"), left
            | left -> (AST.Int(num), left)
        intHelper num stream
    let rec helper toks = function
        | Bracket(b) :: t -> 
            let (tok, next) = parseBrackets b t
            helper (tok::toks) next
        | '\'' :: t -> 
            let (tok, next) = parseChar t
            helper (tok::toks) next
        | '"' :: t -> 
            let (tok, next) = parseString t
            helper (tok::toks) next
        | Digit(d) :: t -> 
            let (tok, next) = parseNumber t d
            helper (tok::toks) next
        | WhiteSpace(_) :: t -> helper toks (parseWs t)
        | 't' :: 'r' :: 'u' :: 'e' :: t -> helper (AST.Bool true :: toks) t
        | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t -> helper (AST.Bool false :: toks) t
        | 'n' :: 'i' :: 'l' :: t ->
            helper (AST.Nil :: toks) t
        | h :: t -> 
            let (tok, next) = parseIdent t h
            helper (tok :: toks) next
        | [] -> List.rev toks
    helper [] (List.ofSeq stream)

