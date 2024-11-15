namespace Lispires
module Main =
    open System
    type Atom = Int of int | Float of float | Symbol of string
    type SExpression = SingleAtom of Atom | Comb of SExpression list 
    let tokenize (input: string) = 
        input
            .Replace("(", " ( ")
            .Replace(")", " ) ")
            .Split()
        |> Array.filter (fun x -> x <> "")
        |> Array.toList

    let parseAtom (token: string): Atom =
        let isInt, intValue = Int32.TryParse token
        if (isInt) then Int intValue 
        else
            let isFloat, floatValue = Double.TryParse token
            if(isFloat) then Float floatValue
            else
                Symbol token
