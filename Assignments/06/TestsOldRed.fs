module TestsOldRed

open Xunit
open Interpreter.Language
open Interpreter.Eval
open Interpreter.State
open Interpreter.Programs
open Result

[<Theory>]
[<InlineData("if")>]
[<InlineData("then")>]
[<InlineData("else")>]
[<InlineData("while")>]
[<InlineData("declare")>]
[<InlineData("print")>]
[<InlineData("random")>]
[<InlineData("fork")>]
[<InlineData("__result__")>]
let TestReservedVariableName (var : string) =
    Assert.Equal<Result<int, error>>(Error (ReservedName var), () |> mkState |> declare var |> bind (getVar var))

[<Theory>]
[<InlineData("hello")>]
[<InlineData("_hello")>]
[<InlineData("_1hello")>]
[<InlineData("h")>]
let TestValidVariableName (var : string) =
    Assert.Equal<Result<int,error>>(Ok 0, () |> mkState |> declare var |> bind (getVar var))

[<Theory>]
[<InlineData("1_hello")>]
[<InlineData("1hello")>]
[<InlineData("")>]
let TestInvalidVariableName (var : string) =
    Assert.Equal<Result<int,error>>(Error (InvalidVarName var), () |> mkState |> declare var |> bind (getVar var))

[<Fact>]
let TestState () =
    Assert.Equal<Result<int, error>>(Error(VarNotDeclared "x"), () |> mkState |> getVar "x")
    Assert.Equal<Result<int, error>>(Ok 0, () |> mkState |> declare "x" |> bind (getVar "x"))

    Assert.Equal<Result<int, error>>(
        Ok 42,
        () |> mkState |> declare "x" |> bind (setVar "x" 42) |> bind (getVar "x")
    )

    Assert.Equal<Result<int, error>>(
        Error(InvalidVarName "1x"),
        () |> mkState |> declare "1x" |> bind (setVar "1x" 42) |> bind (getVar "1x")
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
    Assert.Equal(aexprEval (Num 4) st, aexprEval (Num 4) st)

    Assert.Equal(
        aexprEval (Num 4 .+. Num 2 .*. Num 3) st,
        aexprEval (Num 4 .+. Num 2 .*. Num 3) st
    )

    Assert.Equal(
        aexprEval ((Num 4 .+. Num 2) .*. Num 3) st,
        aexprEval ((Num 4 .+. Num 2) .*. Num 3) st
    )

    Assert.Equal(
        aexprEval ((Num 4 .+. Num 2) ./. Num 0) st,
        aexprEval ((Num 4 .+. Num 2) ./. Num 0) st
    )

    Assert.Equal(
        aexprEval (Num 42 .*. (Num 13 .%. Num 3)) st,
        aexprEval (Num 42 .*. (Num 13 .%. Num 3)) st
    )

    Assert.Equal(
        aexprEval (Num 42 .*. (Num 13 .%. Num 0)) st,
        aexprEval (Num 42 .*. (Num 13 .%. Num 0)) st
    )

[<Fact>]
let TestAExprEval () =
    let emptyState = mkState ()

    let st: state =
        match emptyState |> declare "x" |> bind (setVar "x" 42) with
        | Ok result -> result
        | Error e -> failwith "Error during test setup."

    Assert.Equal<Result<int, error>>(Ok 42, st |> aexprEval (Num 42))
    Assert.Equal<Result<int, error>>(Ok 42, st |> aexprEval (Var "x"))
    Assert.Equal<Result<int, error>>(Error(VarNotDeclared "y"), st |> aexprEval (Var "y"))
    Assert.Equal<Result<int, error>>(Ok 21, st |> aexprEval (Div(Var "x", Num 2)))
    Assert.Equal<Result<int, error>>(Error DivisionByZero, st |> aexprEval (Div(Var "x", Num 0)))

[<Fact>]
let assign1 () =
    let assign x = Seq(Declare "result", Assign("result", Num x))
    Assert.Equal<Result<int, error>>(Ok 1, () |> mkState |> stmntEval (assign 1) |> bind (getVar "result"))


[<Fact>]
let assign2 () =
    let assign x = Seq(Declare "result",
                       Seq(Assign("result", Num x),
                           Assign("result", Num (x + 1))))
    Assert.Equal<Result<int, error>>(Ok 2, () |> mkState |> stmntEval (assign 1) |> bind (getVar "result"))

[<Fact>]
let assign3 () =
    let assign = Seq(Declare "result",
                       If(Not FF, Assign("result", Num 2), Assign("result", Num 2)))
    Assert.Equal<Result<int, error>>(Ok 2, () |> mkState |> stmntEval assign |> bind (getVar "result"))

[<Fact>]
let assign4 () =
    let assign = Seq(Declare "result",
                       If(Not TT, Assign("result", Num 2), Assign("result", Num 2)))
    Assert.Equal<Result<int, error>>(Ok 2, () |> mkState |> stmntEval assign |> bind (getVar "result"))


[<Fact>]
let assign5 () =
    let assign = Seq(Declare "result",
                       If(FF, Assign("result", Num 3), Assign("result", Num 2)))
    Assert.Equal<Result<int, error>>(Ok 2, () |> mkState |> stmntEval assign |> bind (getVar "result"))

[<Fact>]
let assign6 () =
    let assign = Seq(Declare "result",
                       If(TT, Assign("result", Num 3), Assign("result", Num 2)))
    Assert.Equal<Result<int, error>>(Ok 3, () |> mkState |> stmntEval assign |> bind (getVar "result"))

[<Fact>]
let AssignBeforeTrueBranch () =
    let assign = Seq(Declare "result",
                     Seq(Assign("result", Num 5),
                       If(TT, Assign("result", Num 3), Assign("result", Num 2))))
    Assert.Equal<Result<int, error>>(Ok 3, () |> mkState |> stmntEval assign |> bind (getVar "result"))

[<Fact>]
let AssignBeforeFalseBranch () =
    let assign = Seq(Declare "result",
                     Seq(Assign("result", Num 5),
                       If(FF, Assign("result", Num 3), Assign("result", Num 2))))
    Assert.Equal<Result<int, error>>(Ok 2, () |> mkState |> stmntEval assign |> bind (getVar "result"))

[<Fact>]
let AssignBeforeTrueBranchEq () =
    let assign = Seq(Declare "result",
                     Seq(Assign("result", Num 5),
                       If(Eq(Num 1, Num 1), Assign("result", Num 3), Assign("result", Num 2))))
    Assert.Equal<Result<int, error>>(Ok 3, () |> mkState |> stmntEval assign |> bind (getVar "result"))

[<Fact>]
let defaultValue () =
    let assign = Declare "result"
    Assert.Equal<Result<int, error>>(Ok 0, () |> mkState |> stmntEval assign |> bind (getVar "result"))

[<Fact>]
let TestFactorial1 () =
    let res = () |> mkState |> stmntEval (factorial 5) |> bind (getVar "result")
    Assert.Equal<Result<int, error>>(Ok 120, res)

[<Fact>]
let TestFactorial2 () =
    let res = () |> mkState |> stmntEval (factorial2 5) |> bind (getVar "result")
    Assert.Equal<Result<int, error>>(Ok 120, res)

[<Fact>]
let TestFactorialErr1 () =
    Assert.Equal<Result<int, error>>(
        Error(VarAlreadyExists "result"),
        () |> mkState |> stmntEval (factorial_err1 5) |> bind (getVar "result")
    )

[<Fact>]
let TestFactorialErr2 () =
    Assert.Equal<Result<int, error>>(
        Error(VarNotDeclared "y"),
        () |> mkState |> stmntEval (factorial_err2 5) |> bind (getVar "result")
    )

[<Fact>]
let TestFactorialErr3 () =
    Assert.Equal<Result<int, error>>(
        Error DivisionByZero,
        () |> mkState |> stmntEval (factorial_err3 5) |> bind (getVar "result")
    )
