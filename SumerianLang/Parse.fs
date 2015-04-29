module Parse

open FParsec
open FParsec.Primitives
open FParsec.CharParsers

/// The basic terms (or perhaps terminals, take your pick semantically) co
type Term =
    | Int of int
    | Float of float
    | Id of string
    | Definition of string * Term list
    | Bool of bool
    | Block of Term list

/// Parses an Int or Float
let parseNumber : Parser<Term, unit> =
    let numberFormat =     
        NumberLiteralOptions.AllowMinusSign 
        ||| NumberLiteralOptions.AllowFraction 
        ||| NumberLiteralOptions.AllowExponent
    numberLiteral numberFormat "number"
    |>> fun nl ->
            if nl.IsInteger then Int (int nl.String)
            else Float (float nl.String)

/// Chars which consitute valid Ids
let validIdChars = 
    seq { for c in '!'..'~' do if c <> ')' && c <> '(' && c <> '{' && c <> '}' && c <> ':' then yield c }
    |> Seq.filter (System.Char.IsWhiteSpace >> not)
/// Convert a sequence of chars into a string
let toString sequence = new string (Array.ofSeq sequence)
let parseBool = (stringReturn "true" (Bool(true))) <|> (stringReturn "false" (Bool(false)))
let parseId = many1Chars (anyOf validIdChars) |>> Id

// gross hacks for mutually recursive grammar
let parseBlock, private parseBlockRef = createParserForwardedToRef<Term, unit>()
let parseDefinition, private parseDefinitionRef = createParserForwardedToRef<Term, unit>()

let parseTerm = choice [parseNumber; parseBool; parseBlock; parseDefinition; parseId]
let parseExpression : Parser<_,unit> = many (spaces >>. parseTerm .>> spaces)

// the rest of the gross hacks
do parseBlockRef := skipChar '(' >>. parseExpression .>> skipChar ')' |>> Block 
do parseDefinitionRef := 
    attempt (many1Chars (anyOf validIdChars) .>> spaces .>> skipString ":") // parse definition
    .>> spaces .>> skipChar '{' .>>. parseExpression .>> skipChar '}' // parse block
    |>> Definition // convert to Term

let parseToEnd = parseExpression .>> (spaces >>. eof) // A parse that will continue to the end of the file

/// Parse a string into a list of Terms
let parse (str:string) = 
    match run parseToEnd str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith (sprintf "Parse Error: %s" errorMsg)

