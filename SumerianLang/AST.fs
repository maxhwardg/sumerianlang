module AST


type Bracket = 
    | Paren
    | Curly
    | Square
    static member openOf = function
        | Paren -> '('
        | Curly -> '{'
        | Square -> '['
    static member closeOf = function
        | Paren -> ')'
        | Curly -> '}'
        | Square -> ']'

type Node = 
    //basic/atomic nodes
    | Id of string
    | String of string
    | Char of char
    | Int of int32
    | Bool of bool
    | Block of Bracket * Node list
    | Nil
    | Float of double
    //special error type
    | Error of string