module YellowTests

open Xunit
open Interpreter.Language
open Interpreter.Eval
open Interpreter.State
open Result

[<Fact>]
let TestReservedVariableName () =
    Assert.False(reservedVariableName "hello")
    Assert.True(reservedVariableName "if")
    Assert.True(reservedVariableName "then")
    Assert.True(reservedVariableName "else")
    Assert.True(reservedVariableName "while")
    Assert.True(reservedVariableName "declare")
    Assert.True(reservedVariableName "print")
    Assert.True(reservedVariableName "random")
    Assert.True(reservedVariableName "fork")
    Assert.True(reservedVariableName "__result__")
    Assert.False(reservedVariableName "")
    Assert.False(reservedVariableName "i")

[<Fact>]
let TestValidVariableName () =
    Assert.True(validVariableName "hello")
    Assert.True(validVariableName "_hello")
    Assert.True(validVariableName "_1hello")
    Assert.False(validVariableName "1_hello")
    Assert.False(validVariableName "1hello")
    Assert.True(validVariableName "h")
    Assert.False(validVariableName "")

[<Fact>]
let TestState () =
    Assert.Equal<Result<int,error>>(Error (VarNotDeclared "x"), () |> mkState |> getVar "x")
    Assert.Equal<Result<int,error>>(Ok 0, () |> mkState |> declare "x" |> bind (getVar "x"))

    Assert.Equal<Result<int,error>>(
        Ok 42,
        ()
        |> mkState
        |> declare "x"
        |> bind (setVar "x" 42)
        |> bind (getVar "x")
    )

    Assert.Equal<Result<int,error>>(
        Error (InvalidVarName "1x"),
        ()
        |> mkState
        |> declare "1x"
        |> bind (setVar "1x" 42)
        |> bind (getVar "1x")
    )

[<Fact>]
let AExprEvalOld () =
    let st = mkState ()
    Assert.Equal(Ok 4, aexprEval (Num 4) st)
    Assert.Equal(Ok 10, aexprEval (Num 4 .+. Num 2 .*. Num 3) st)
    Assert.Equal(Ok 18, aexprEval ((Num 4 .+. Num 2) .*. Num 3) st)
    Assert.Equal(Error DivisionByZero, aexprEval ((Num 4 .+. Num 2) ./. Num 0) st)
    Assert.Equal(Ok 42, aexprEval (Num 42 .*. (Num 13 .%. Num 3)) st)
    Assert.Equal(Error DivisionByZero, aexprEval (Num 42 .*. (Num 13 .%. Num 0)) st)

[<Fact>]
let AExprEvalOld2 () =
    let st = mkState ()
    Assert.Equal(aexprEval (Num 4) st, aexprEval2 (Num 4) st)
    Assert.Equal(aexprEval (Num 4 .+. Num 2 .*. Num 3) st, aexprEval2 (Num 4 .+. Num 2 .*. Num 3) st)
    Assert.Equal(aexprEval ((Num 4 .+. Num 2) .*. Num 3) st, aexprEval2 ((Num 4 .+. Num 2) .*. Num 3) st)
    Assert.Equal(aexprEval ((Num 4 .+. Num 2) ./. Num 0) st, aexprEval2 ((Num 4 .+. Num 2) ./. Num 0) st)

    Assert.Equal(
        aexprEval (Num 42 .*. (Num 13 .%. Num 3)) st,
        aexprEval2 (Num 42 .*. (Num 13 .%. Num 3)) st
    )

    Assert.Equal(
        aexprEval (Num 42 .*. (Num 13 .%. Num 0)) st,
        aexprEval2 (Num 42 .*. (Num 13 .%. Num 0)) st
    )

[<Fact>]
let TestAExprEval () =
    let emptyState = mkState ()
    let st : state =
        match emptyState |> declare "x" |> bind (setVar "x" 42) with
        | Ok result -> result
        | Error e -> failwith "Error during test setup."

    Assert.Equal<Result<int,error>>(Ok 42, st |> aexprEval (Num 42))
    Assert.Equal<Result<int,error>>(Ok 42, st |> aexprEval (Var "x"))
    Assert.Equal<Result<int,error>>(Error (VarNotDeclared "y"), st |> aexprEval (Var "y"))
    Assert.Equal<Result<int,error>>(Ok 21, st |> aexprEval (Div (Var "x", Num 2)))
    Assert.Equal<Result<int,error>>(Error DivisionByZero, st |> aexprEval (Div (Var "x", Num 0)))


[<Fact>]
let TestAExprEval2 () =
    let emptyState = mkState ()
    let st : state =
        match emptyState |> declare "x" |> bind (setVar "x" 42) with
        | Ok result -> result
        | Error e -> failwith "Error during test setup."

    Assert.Equal<Result<int,error>>(st |> aexprEval (Num 42), st |> aexprEval2 (Num 42))
    Assert.Equal<Result<int,error>>(st |> aexprEval (Var "x"), st |> aexprEval2 (Var "x"))
    Assert.Equal<Result<int,error>>(st |> aexprEval (Var "y"), st |> aexprEval2 (Var "y"))
    Assert.Equal<Result<int,error>>(st |> aexprEval (Div (Var "x", Num 2)), st |> aexprEval2 (Div (Var "x", Num 2)))
    Assert.Equal<Result<int,error>>(st |> aexprEval (Div (Var "x", Num 0)), st |> aexprEval2 (Div (Var "x", Num 0)))