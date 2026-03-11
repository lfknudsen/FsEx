module Tests

open System
open Interpreter.Eval
open Interpreter.Language
open Xunit

[<Fact>]
let ``ArithExprToString1`` () =
    Assert.Equal("0", aexprToString (Num 0))
    Assert.Equal("1", aexprToString (Num 1))
    Assert.Equal("2", aexprToString (Num 2))
    Assert.Equal("-2", aexprToString (Num -2))
    Assert.Equal("(1 + 2)", aexprToString (Add(Num 1, Num 2)))
    Assert.Equal("(4 + (2 * 3))", aexprToString (Num 4 .+. Num 2 .*. Num 3))
    Assert.Equal("((4 + 2) * 3)", aexprToString ((Num 4 .+. Num 2) .*. Num 3))
    Assert.Equal("((18 / 3) / 2)", aexprToString ((Num 18 ./. Num 3) ./. Num 2))
    Assert.Equal("(18 / (3 / 2))", aexprToString (Num 18 ./. (Num 3 ./. Num 2)))

    Assert.Equal(
        "(42 * (13 + (((13 / 3) * 3) * -1)))",
        aexprToString (Num 42 .*. (Num 13 .%. Num 3))
    )

[<Fact>]
let ``BoolExprToString1`` () =
    Assert.Equal("true", bexprToString TT)
    Assert.Equal("(not true)", bexprToString FF)
    Assert.Equal("(42 = 32)", bexprToString (Num 42 .=. Num 32))
    Assert.Equal("(42 < (32 + 10))", bexprToString (Num 42 .<. Num 32 .+. Num 10))

    Assert.Equal(
        "(not ((not (42 < (32 + 10))) /\ (not (not (32 < (27 + (((27 / 25) * 25) * -1)))))))",
        bexprToString ((Num 42 .<. Num 32 .+. Num 10) .||. (Num 32 .>=. Num 27 .%. Num 25))
    )

[<Fact>]
let AExprEval () =
    Assert.Equal(Some 4, aexprEval (Num 4))
    Assert.Equal(Some 10, aexprEval (Num 4 .+. Num 2 .*. Num 3))
    Assert.Equal(Some 18, aexprEval ((Num 4 .+. Num 2) .*. Num 3))
    Assert.Equal(None, aexprEval ((Num 4 .+. Num 2) ./. Num 0))
    Assert.Equal(Some 42, aexprEval (Num 42 .*. (Num 13 .%. Num 3)))
    Assert.Equal(None, aexprEval (Num 42 .*. (Num 13 .%. Num 0)))

[<Fact>]
let AExprEval2 () =
    Assert.Equal(aexprEval (Num 4), aexprEval2 (Num 4))
    Assert.Equal(aexprEval (Num 4 .+. Num 2 .*. Num 3), aexprEval2 (Num 4 .+. Num 2 .*. Num 3))
    Assert.Equal(aexprEval ((Num 4 .+. Num 2) .*. Num 3), aexprEval2 ((Num 4 .+. Num 2) .*. Num 3))
    Assert.Equal(aexprEval ((Num 4 .+. Num 2) ./. Num 0), aexprEval2 ((Num 4 .+. Num 2) ./. Num 0))

    Assert.Equal(
        aexprEval (Num 42 .*. (Num 13 .%. Num 3)),
        aexprEval2 (Num 42 .*. (Num 13 .%. Num 3))
    )

    Assert.Equal(
        aexprEval (Num 42 .*. (Num 13 .%. Num 0)),
        aexprEval2 (Num 42 .*. (Num 13 .%. Num 0))
    )

[<Fact>]
let ``ArithExprToString2`` () =
    Assert.Equal("0", aexprToString2 (Num 0))
    Assert.Equal("1", aexprToString2 (Num 1))
    Assert.Equal("2", aexprToString2 (Num 2))
    Assert.Equal("-2", aexprToString2 (Num -2))
    Assert.Equal("1 + 2", aexprToString2 (Add(Num 1, Num 2)))
    Assert.Equal("4 + 2 * 3", aexprToString2 (Num 4 .+. Num 2 .*. Num 3))
    Assert.Equal("(4 + 2) * 3", aexprToString2 ((Num 4 .+. Num 2) .*. Num 3))
    Assert.Equal("18 / 3 / 2", aexprToString2 ((Num 18 ./. Num 3) ./. Num 2))
    Assert.Equal("18 / (3 / 2)", aexprToString2 (Num 18 ./. (Num 3 ./. Num 2)))
    Assert.Equal("42 * (13 + 13 / 3 * 3 * -1)", aexprToString2 (Num 42 .*. (Num 13 .%. Num 3)))

[<Fact>]
let ``BoolExprToString2`` () =
    Assert.Equal("true", bexprToString2 TT)
    Assert.Equal("not true", bexprToString2 FF)
    Assert.Equal("42 = 32", bexprToString2 (Num 42 .=. Num 32))
    Assert.Equal("42 < 32 + 10", bexprToString2 (Num 42 .<. Num 32 .+. Num 10))

    Assert.Equal(
        "not (not (42 < 32 + 10) /\ not (not (32 < 27 + 27 / 25 * 25 * -1)))",
        bexprToString2 ((Num 42 .<. Num 32 .+. Num 10) .||. (Num 32 .>=. Num 27 .%. Num 25))
    )

[<Fact>]
let AexprFold () =
    let simpleEval = aexprFold id (+) (*) (/)
    Assert.Equal(42, simpleEval (Num 42))
    Assert.Equal(21, simpleEval (Num 42 ./. Num 2))
    Assert.Throws<DivideByZeroException>(fun () -> simpleEval (Num 42 ./. Num 0) |> ignore)

[<Fact>]
let AExprEval3 () =
    Assert.Equal(aexprEval (Num 4), aexprEval3 (Num 4))
    Assert.Equal(aexprEval (Num 4 .+. Num 2 .*. Num 3), aexprEval3 (Num 4 .+. Num 2 .*. Num 3))
    Assert.Equal(aexprEval ((Num 4 .+. Num 2) .*. Num 3), aexprEval3 ((Num 4 .+. Num 2) .*. Num 3))
    Assert.Equal(aexprEval ((Num 4 .+. Num 2) ./. Num 0), aexprEval3 ((Num 4 .+. Num 2) ./. Num 0))

    Assert.Equal(
        aexprEval (Num 42 .*. (Num 13 .%. Num 3)),
        aexprEval3 (Num 42 .*. (Num 13 .%. Num 3))
    )

    Assert.Equal(
        aexprEval (Num 42 .*. (Num 13 .%. Num 0)),
        aexprEval3 (Num 42 .*. (Num 13 .%. Num 0))
    )

[<Fact>]
let ``ArithExprToString3`` () =
    Assert.Equal("0", aexprToString3 (Num 0))
    Assert.Equal("1", aexprToString3 (Num 1))
    Assert.Equal("2", aexprToString3 (Num 2))
    Assert.Equal("-2", aexprToString3 (Num -2))
    Assert.Equal("(1 + 2)", aexprToString3 (Add(Num 1, Num 2)))
    Assert.Equal("(4 + (2 * 3))", aexprToString3 (Num 4 .+. Num 2 .*. Num 3))
    Assert.Equal("((4 + 2) * 3)", aexprToString3 ((Num 4 .+. Num 2) .*. Num 3))
    Assert.Equal("((18 / 3) / 2)", aexprToString3 ((Num 18 ./. Num 3) ./. Num 2))
    Assert.Equal("(18 / (3 / 2))", aexprToString3 (Num 18 ./. (Num 3 ./. Num 2)))

    Assert.Equal(
        "(42 * (13 + (((13 / 3) * 3) * -1)))",
        aexprToString3 (Num 42 .*. (Num 13 .%. Num 3))
    )

[<Fact>]
let ``ArithExprToString4`` () =
    Assert.Equal("0", aexprToString4 (Num 0))
    Assert.Equal("1", aexprToString4 (Num 1))
    Assert.Equal("2", aexprToString4 (Num 2))
    Assert.Equal("-2", aexprToString4 (Num -2))
    Assert.Equal("1 + 2", aexprToString4 (Add(Num 1, Num 2)))
    Assert.Equal("4 + 2 * 3", aexprToString4 (Num 4 .+. Num 2 .*. Num 3))
    Assert.Equal("(4 + 2) * 3", aexprToString4 ((Num 4 .+. Num 2) .*. Num 3))
    Assert.Equal("18 / 3 / 2", aexprToString4 ((Num 18 ./. Num 3) ./. Num 2))
    Assert.Equal("18 / (3 / 2)", aexprToString4 (Num 18 ./. (Num 3 ./. Num 2)))
    Assert.Equal("42 * (13 + 13 / 3 * 3 * -1)", aexprToString4 (Num 42 .*. (Num 13 .%. Num 3)))

[<Fact>]
let ``BexprFold`` () =
    let simpleAEval = aexprFold id (+) (*) (/)
    let simpleBEval = bexprFold simpleAEval true (=) (<) (&&) not
    Assert.Equal(true, simpleBEval TT)
    Assert.Equal(true, simpleBEval (Num 4 .>. Num 32 .%. Num 2))

    Assert.Throws<DivideByZeroException>(fun () ->
        simpleBEval (Num 4 .>. Num 32 .%. Num 0) |> ignore)

[<Fact>]
let ``BoolExprToString3`` () =
    Assert.Equal("true", bexprToString3 TT)
    Assert.Equal("(not true)", bexprToString3 FF)
    Assert.Equal("(42 = 32)", bexprToString3 (Num 42 .=. Num 32))
    Assert.Equal("(42 < (32 + 10))", bexprToString3 (Num 42 .<. Num 32 .+. Num 10))

    Assert.Equal(
        "(not ((not (42 < (32 + 10))) /\ (not (not (32 < (27 + (((27 / 25) * 25) * -1)))))))",
        bexprToString3 ((Num 42 .<. Num 32 .+. Num 10) .||. (Num 32 .>=. Num 27 .%. Num 25))
    )

[<Fact>]
let BoolExprEval1 () =
    Assert.Equal(Some true, bexprEval TT)
    Assert.Equal(Some false, bexprEval FF)
    Assert.Equal(Some false, bexprEval (Num 42 .=. Num 32))
    Assert.Equal(Some false, bexprEval (Num 42 .<. Num 32 .+. Num 10))

    Assert.Equal(
        Some true,
        bexprEval ((Num 42 .<. Num 32 .+. Num 10) .||. (Num 32 .>=. Num 27 .%. Num 25))
    )

[<Fact>]
let BoolExprEval2 () =
    Assert.Equal(Some true, bexprEval3 TT)
    Assert.Equal(Some false, bexprEval3 FF)
    Assert.Equal(Some false, bexprEval3 (Num 42 .=. Num 32))
    Assert.Equal(Some false, bexprEval3 (Num 42 .<. Num 32 .+. Num 10))

    Assert.Equal(
        Some true,
        bexprEval3 ((Num 42 .<. Num 32 .+. Num 10) .||. (Num 32 .>=. Num 27 .%. Num 25))
    )


[<Fact>]
let ``BoolExprToString4`` () =
    Assert.Equal("true", bexprToString4 TT)
    Assert.Equal("not true", bexprToString4 FF)
    Assert.Equal("42 = 32", bexprToString4 (Num 42 .=. Num 32))
    Assert.Equal("42 < 32 + 10", bexprToString4 (Num 42 .<. Num 32 .+. Num 10))

    Assert.Equal(
        "not (not (42 < 32 + 10) /\ not (not (32 < 27 + 27 / 25 * 25 * -1)))",
        bexprToString4 ((Num 42 .<. Num 32 .+. Num 10) .||. (Num 32 .>=. Num 27 .%. Num 25))
    )
