namespace Lispires
module Main =
    open Parser
    "(+ 1 2 3 (* 2 3))" |> charListFromString |> runParser parseExpression |> printfn "%A"
