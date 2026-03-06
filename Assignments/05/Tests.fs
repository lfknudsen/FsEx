module Tests

open Interpreter
open Interpreter.Language
open Interpreter.StateGreen
open Option
open Xunit

[<Fact>]
let TestReservedVariableNameGreen () =
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
let TestValidVariableNameGreen () =
    Assert.True(validVariableName "hello")
    Assert.True(validVariableName "_hello")
    Assert.True(validVariableName "_1hello")
    Assert.False(validVariableName "1_hello")
    Assert.False(validVariableName "1hello")
    Assert.True(validVariableName "h")
    Assert.False(validVariableName "")

[<Fact>]
let TestStateGreen () =
    Assert.Equal<int option>(None, () |> StateGreen.mkState |> getVar "x")
    Assert.Equal<int option>(Some 0, () |> StateGreen.mkState |> declare "x" |> bind (getVar "x"))

    Assert.Equal<int option>(
        Some 42,
        ()
        |> StateGreen.mkState
        |> declare "x"
        |> bind (setVar "x" 42)
        |> bind (getVar "x")
    )

    Assert.Equal<int option>(
        None,
        ()
        |> StateGreen.mkState
        |> declare "1x"
        |> bind (setVar "1x" 42)
        |> bind (getVar "1x")
    )

[<Fact>]
let AExprEvalOldGreen () =
    let st = StateGreen.mkState ()
    Assert.Equal(Some 4, EvalGreen.aexprEval (Num 4) st)
    Assert.Equal(Some 10, EvalGreen.aexprEval (Num 4 .+. Num 2 .*. Num 3) st)
    Assert.Equal(Some 18, EvalGreen.aexprEval ((Num 4 .+. Num 2) .*. Num 3) st)
    Assert.Equal(None, EvalGreen.aexprEval ((Num 4 .+. Num 2) ./. Num 0) st)
    Assert.Equal(Some 42, EvalGreen.aexprEval (Num 42 .*. (Num 13 .%. Num 3)) st)
    Assert.Equal(None, EvalGreen.aexprEval (Num 42 .*. (Num 13 .%. Num 0)) st)

[<Fact>]
let AExprEvalOld2Green () =
    let st = StateGreen.mkState ()
    Assert.Equal(EvalGreen.aexprEval (Num 4) st, EvalGreen.aexprEval2 (Num 4) st)
    Assert.Equal(EvalGreen.aexprEval (Num 4 .+. Num 2 .*. Num 3) st, EvalGreen.aexprEval2 (Num 4 .+. Num 2 .*. Num 3) st)
    Assert.Equal(EvalGreen.aexprEval ((Num 4 .+. Num 2) .*. Num 3) st, EvalGreen.aexprEval2 ((Num 4 .+. Num 2) .*. Num 3) st)
    Assert.Equal(EvalGreen.aexprEval ((Num 4 .+. Num 2) ./. Num 0) st, EvalGreen.aexprEval2 ((Num 4 .+. Num 2) ./. Num 0) st)

    Assert.Equal(
        EvalGreen.aexprEval (Num 42 .*. (Num 13 .%. Num 3)) st,
        EvalGreen.aexprEval2 (Num 42 .*. (Num 13 .%. Num 3)) st
    )

    Assert.Equal(
        EvalGreen.aexprEval (Num 42 .*. (Num 13 .%. Num 0)) st,
        EvalGreen.aexprEval2 (Num 42 .*. (Num 13 .%. Num 0)) st
    )

[<Fact>]
let TestAExprEvalGreen () =
    let emptyState = StateGreen.mkState ()
    let st = emptyState |> declare "x" |> bind (setVar "x" 42) |> get

    Assert.Equal<int option>(Some 42, st |> EvalGreen.aexprEval (Num 42))
    Assert.Equal<int option>(Some 42, st |> EvalGreen.aexprEval (Var "x"))
    Assert.Equal<int option>(None, st |> EvalGreen.aexprEval (Var "y"))
    Assert.Equal<int option>(Some 21, st |> EvalGreen.aexprEval (Div (Var "x", Num 2)))
    Assert.Equal<int option>(None, st |> EvalGreen.aexprEval (Div (Var "x", Num 0)))


[<Fact>]
let TestAExprEval2Green () =
    let emptyState = StateGreen.mkState ()
    let st = emptyState |> declare "x" |> bind (setVar "x" 42) |> get

    Assert.Equal<int option>(st |> EvalGreen.aexprEval (Num 42), st |> EvalGreen.aexprEval2 (Num 42))
    Assert.Equal<int option>(st |> EvalGreen.aexprEval (Var "x"), st |> EvalGreen.aexprEval2 (Var "x"))
    Assert.Equal<int option>(st |> EvalGreen.aexprEval (Var "y"), st |> EvalGreen.aexprEval2 (Var "y"))
    Assert.Equal<int option>(st |> EvalGreen.aexprEval (Div (Var "x", Num 2)), st |> EvalGreen.aexprEval2 (Div (Var "x", Num 2)))
    Assert.Equal<int option>(st |> EvalGreen.aexprEval (Div (Var "x", Num 0)), st |> EvalGreen.aexprEval2 (Div (Var "x", Num 0)))