namespace Lispires
open System
module Parser =

    type Parser<'a> = Parser of (char list -> (char list * 'a) option)
    
    let charListFromString (str: string) = str |> Seq.toList
    let createParser expectedChar =
        let lambda inputChars =
            match inputChars with
            | c :: remaining when c = expectedChar -> Some (remaining, c)
            | _ -> None
        Parser lambda
    let runParser parser inputChars =
        let (Parser parserFunc) = parser
        parserFunc inputChars
    let orParse p1 p2 =
        let lambda input =
            match runParser p1 input with
            | Some result -> Some result
            | None ->
                match runParser p2 input with
                | Some result -> Some result
                | None -> None
        Parser lambda
        
    let (<|>) = orParse

    let choice parserList =
        parserList
        |> List.reduce orParse
    let anyOf charList =
        charList
        |> List.map createParser
        |> choice
    let andParse parser1 parser2  =
        let lambda input =
            match runParser parser1 input with
                | None -> None
                | Some (p1Remaining, p1Char) -> 
                    match runParser parser2 p1Remaining with
                        | None -> None
                        | Some (p2Remaining, p2Char) 
                            -> Some (p2Remaining, (p1Char, p2Char)) 
        Parser lambda
    let (.>>.) = andParse
    
    let mapParser mapFunc parser =
        let lambda inputChars =
            match runParser parser inputChars with
            | None -> None
            | Some (remaining, result) ->
                Some (remaining, mapFunc result)
        Parser lambda
    let applyParser parserFunction parserParameter =
        (parserFunction .>>. parserParameter)
        |> mapParser (fun (parsedFunc, parsedValue) -> parsedFunc parsedValue)
    let (<*>) = applyParser

    let andThen parserA parserB =
        let lambda input =
            match runParser parserA input with
            | None -> None
            | Some (remaining, char) ->
                match runParser parserB remaining with
                | None -> None
                | Some result -> Some result
        Parser lambda
    let (.*>.) = andThen
    let sequenceLeft parserA parserB =
        let lambda input =
            match runParser parserA input with
            | None -> None
            | Some (remaining, char) ->
                match runParser parserB remaining with
                | None -> None
                | Some (remainingB, _) -> Some (remainingB, char)
        Parser lambda
    let (.<*.) = sequenceLeft
    let pureParser c =
        let lambda inputChars = Some (inputChars, c)
        Parser lambda

    let liftToParser2 funcToLift paramAsParser1 paramAsParser2 =
            (pureParser funcToLift) <*> paramAsParser1 <*> paramAsParser2

    let rec sequenceParser parserList  =
        let unshiftParser = liftToParser2 (fun head rest -> head :: rest)
        match parserList with
        | [] -> pureParser []
        | parser :: remainingParsers ->
            unshiftParser parser (sequenceParser remainingParsers)

    let createStringParser str =
        str
        |> charListFromString
        |> List.map createParser
        |> sequenceParser

    let rec parseZeroOrMore 
        (parser: Parser<char>) input (acc: (char list * char list) )=
        let parseResult = runParser parser input    
        match parseResult with
        | Some (remainingChars, parsedChar)
            ->
                parseZeroOrMore parser remainingChars (remainingChars, (parsedChar :: (snd acc)))
        | None ->
            let (remaining, parsedChars) = acc
            (remaining, List.rev parsedChars)
    let many parser =
        let lambda input =
            Some (parseZeroOrMore parser input (input,[]))
        Parser lambda
    
    let parseInt = many (anyOf ['0'..'9'])
    let charSet = 
        ['a'..'z'] @ ['A'..'Z']
    let parseSymbol = many (anyOf charSet)
    
    let parseSpace = many (anyOf [' '; '\n'; '\r'])

    let parseOpening = createParser '('
    let parseClosing = createParser ')'

    let parseDefinition = 
        parseOpening .*>. parseSpace .*>. parseSymbol .<*. parseSpace .<*. parseClosing
    
    printfn "Testando" |> ignore
    "(                   Pedro  ) "
    |> charListFromString
    |> runParser parseDefinition
    |> printfn "%A"