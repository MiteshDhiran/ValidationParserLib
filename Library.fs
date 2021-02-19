namespace ValidationParserLib

open System
open System.Runtime.InteropServices
open FParsec
open FParsec.Error
open FParsec.Primitives

module ValidationRule  =

    type BinaryExprKind =
        | Add
        | Subtract
        | Multiply
        | Divide
        | And
        | Or
        | Equals
        | NotEquals
        | GreaterThan
        | GreaterThanOrEquals
        | LesserThan
        | LesserThanOrEquals
        
    

    type LeftBracket = LeftBracket
    type RightBracket = RightBracket 
    type Expr =
        | IntLiteral of int
        | FloatLiteral of float
        | StringLiteral of string
        | Identifier of string
        | Binary of (Expr * Expr * BinaryExprKind)
        | Bracket of Expr
        | IsNUll of Expr
        | IsNotNull of Expr

    type Stmt =
        | Condition of Expr
    
    let ws = skipMany (skipChar ' ')
    let ws1 : Parser<unit,unit> = skipMany1 (skipChar ' ')

    let quote : Parser<_, unit> = skipChar '\''

    let intOrFloatLiteral =
        numberLiteral (NumberLiteralOptions.DefaultFloat ||| NumberLiteralOptions.DefaultInteger) "number"
        |>> fun n ->
                if n.IsInteger then Expr.IntLiteral (int n.String)
                else Expr.FloatLiteral (float n.String)
        .>> ws
 
    let stringLiteral = quote >>. manyCharsTill anyChar quote |>> Expr.StringLiteral .>> ws

    let identifier = many1Chars (letter <|> digit) |>> Expr.Identifier .>> ws

    
    let leftBracket : Parser<_,unit> = skipChar '(' 
    let rightBracket : Parser<_,unit> = skipChar ')'
    

    
    let parseExpression, implementation = createParserForwardedToRef ()
    let parseExpressionNull, implementationNULL = createParserForwardedToRef ()


    let isNullParser = parseExpressionNull |>> (fun n -> Expr.IsNUll n) .>> pstring "IS NULL" .>> ws
    let isNotNullParser = parseExpressionNull |>> (fun n -> Expr.IsNotNull n) .>> pstring "IS NOT NULL" .>> ws
    
    let parseExpressionsPair =
        between 
            (leftBracket)
            (rightBracket)
            parseExpression |>> (fun n -> Expr.Bracket n) .>> ws

    let opp = OperatorPrecedenceParser<Expr, _, _>()
                
    opp.TermParser <- choice [
        intOrFloatLiteral
        stringLiteral
        identifier
        isNullParser
        parseExpressionsPair
    ]
    
    (*type IsNullString = IsNullString of string*)
    

    opp.AddOperator <| InfixOperator("*", ws, 13, Associativity.Left, fun x y -> Expr.Binary (x, y, BinaryExprKind.Multiply))
    opp.AddOperator <| InfixOperator("/", ws, 2, Associativity.Left, fun x y -> Expr.Binary (x, y, BinaryExprKind.Divide))
    opp.AddOperator <| InfixOperator("-", ws, 3, Associativity.Left, fun x y -> Expr.Binary (x, y, BinaryExprKind.Subtract))
    opp.AddOperator <| InfixOperator("+", ws, 4, Associativity.Left, fun x y -> Expr.Binary (x, y, BinaryExprKind.Add))
    opp.AddOperator <| InfixOperator("&&", ws, 5, Associativity.Left, fun x y -> Expr.Binary (x, y, BinaryExprKind.And))
    opp.AddOperator <| InfixOperator("||", ws, 6, Associativity.Left, fun x y -> Expr.Binary (x, y, BinaryExprKind.Or))
    opp.AddOperator <| InfixOperator("=", ws, 7, Associativity.None, fun x y -> Expr.Binary (x, y, BinaryExprKind.Equals))
    opp.AddOperator <| InfixOperator("!=", ws, 8, Associativity.None, fun x y -> Expr.Binary (x, y, BinaryExprKind.NotEquals))
    opp.AddOperator <| InfixOperator(">", ws, 9, Associativity.None, fun x y -> Expr.Binary (x, y, BinaryExprKind.GreaterThan))
    opp.AddOperator <| InfixOperator(">=", ws, 10, Associativity.None, fun x y -> Expr.Binary (x, y, BinaryExprKind.GreaterThanOrEquals))
    opp.AddOperator <| InfixOperator("<", ws, 11, Associativity.None, fun x y -> Expr.Binary (x, y, BinaryExprKind.LesserThan))
    opp.AddOperator <| InfixOperator("<=", ws, 12, Associativity.None, fun x y -> Expr.Binary (x, y, BinaryExprKind.LesserThanOrEquals))
    opp.AddOperator <| PostfixOperator("IS NULL", ws , 13, true, fun x -> Expr.IsNUll x)
    opp.AddOperator <| PostfixOperator("IS NOT NULL", ws , 13, true, fun x -> Expr.IsNotNull x)
    

    let ruleExpr = opp.ExpressionParser

    implementation := opp.ExpressionParser

    (*implementationNULL := opp.ExpressionParser*)
    implementationNULL := choice [
        intOrFloatLiteral
        stringLiteral
        identifier
    ]    
    let condition = ws >>. ruleExpr .>> ws 

    let parseRule input =
        match run condition input with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err

    (*run parseExpressionsPair ruleInput*)
    let getParserResult ruleInput = 
       let result = run condition ruleInput
       match result with
            | Success (res, _, _) -> Result.Ok res
            | Failure (err, _, _) -> Result.Error err
                   
       
    let execute query source =
        let rec evaluate expr element =
            match expr with
                | Bracket bracketExp -> evaluate bracketExp element     
                | IntLiteral i -> i :> obj
                | FloatLiteral f -> f :> obj
                | StringLiteral s -> s :> obj
                | Identifier n -> element.GetType().GetProperty(n).GetValue(element)
                | IsNUll e ->
                        let evaluatedValue = evaluate e element
                        match evaluatedValue with
                            | null -> true :>obj
                            | _ -> false :>obj
                | IsNotNull e ->
                        let evaluatedValue = evaluate e element
                        match evaluatedValue with
                            | null -> false :>obj
                            | _ -> true :>obj
                | Binary (left, right, kind) ->
                let leftEvaluated = evaluate left element
                let rightEvaluated = evaluate right element

                match kind with
                | Add -> (leftEvaluated :?> float) + (rightEvaluated :?> float) :> obj
                | Subtract -> (leftEvaluated :?> float) - (rightEvaluated :?> float) :> obj
                | Multiply -> (leftEvaluated :?> float) * (rightEvaluated :?> float) :> obj
                | Divide -> (leftEvaluated :?> float) / (rightEvaluated :?> float) :> obj
                | And -> (leftEvaluated :?> bool && rightEvaluated:?> bool) :> obj
                | Or -> (leftEvaluated :?> bool || rightEvaluated:?> bool) :> obj
                | Equals -> leftEvaluated = rightEvaluated :> obj
                | NotEquals -> not (leftEvaluated = rightEvaluated) :> obj
                | GreaterThan -> (leftEvaluated :?> IComparable) > (rightEvaluated :?> IComparable) :> obj
                | GreaterThanOrEquals -> (leftEvaluated :?> IComparable) >= (rightEvaluated :?> IComparable) :> obj
                | LesserThan -> (leftEvaluated :?> IComparable) < (rightEvaluated :?> IComparable) :> obj
                | LesserThanOrEquals -> (leftEvaluated :?> IComparable) <= (rightEvaluated :?> IComparable) :> obj
    
        //evaluate query source :?> bool
        evaluate query source

     