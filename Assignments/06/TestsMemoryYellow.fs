module _06.TestsMemoryYellow

open Interpreter.Eval
open Interpreter.Language
open Interpreter.State
open Xunit
open Interpreter.MemoryYellow

[<Fact>]
let TestEmpty () =
    let mem = (empty 0 |> alloc 0)
    Assert.Equal(Error (NegativeMemoryAllocated 0), mem)

let create size : state =
    mkState size

[<Fact>]
let TestFreePtrLowerThanNextPtrPlusSizeGreaterThanNext () =
    let size = 20
    let st = create size
    let freed = Interpreter.State.free 10 30 st
    Assert.True(freed.IsError)

[<Fact>]
let AllocMemRead () =
    let state = create 10
    let result =
        state
        |> stmntEval (Seq (Declare "x", Seq(Alloc ("x", Num 3), Seq(Declare "result",
            Assign ("result",
            MemRead (Var "x"))))))
        |> Result.bind (getVar "result")

    Assert.Equal(Ok 0, result)

[<Fact>]
let Alloc2 () =
    let result =
        create 10
        |> stmntEval (Seq (Declare "x", Seq(Alloc ("x", Num 3),
                                        Seq(MemWrite(Var "x", Num 42), Seq(Declare "result",
                                                                           Assign("result", MemRead (Var "x")))))))
        |> Result.bind (getVar "result")
    Assert.Equal(Ok 42, result)

[<Fact>]
let Alloc3 () =
    let result = 10
                 |> mkState
                 |> stmntEval (Seq (Declare "x", Seq(Alloc ("x", Num 3), Seq(MemWrite(Var "x", Num
                        42), Seq(Declare "result", Assign("result", MemRead (Var "x" .+. Num 1)))))))
                 |> Result.bind (getVar "result")
    Assert.Equal(Ok 0, result)

[<Fact>]
let Alloc4 () =
    let result = 10 |> mkState
                 |> stmntEval (Seq (Declare "x", Seq(Alloc ("x", Num 3), Seq(MemWrite(Var "x", Num
                        42), Seq(Declare "result", Assign("result", MemRead (Var "x" .+. Num 3)))))))
                 |> Result.bind (getVar "result")

    Assert.Equal<Result<int,error>>(Error (MemoryNotAllocated 3), result)

[<Fact>]
let Alloc5 () =
    let result = 10 |> mkState
                 |> stmntEval (Seq (Declare "x", Seq(Alloc ("x", Num 3), Seq(MemWrite(Var "x", Num
                        42), Seq(Free(Var "x" .+. Num 1, Num 2),
                                 Seq(Declare "result",
                                     Assign("result", MemRead (Var "x"))))))))
                 |> Result.bind (getVar "result")

    Assert.Equal<Result<int,error>>(Ok 42, result)


[<Fact>]
let Alloc6 () =
    let result = 10 |> mkState
                 |> stmntEval (Seq (Declare "x", Seq(Alloc ("x", Num 3),
                                                     Seq(MemWrite(Var "x", Num 42),
                                                         Seq(Free(Var "x", Num 2),
                                 Seq(Declare "result",
                                     Assign("result", MemRead (Var "x"))))))))
                 |> Result.bind (getVar "result")

    Assert.Equal<Result<int,error>>(Error (MemoryNotAllocated 0), result)


[<Fact>]
let Alloc7 () =
    let result = 10 |> mkState
                 |> stmntEval (Seq (Declare "x", Seq(Alloc ("x", Num 3),
                                                     Seq(MemWrite(Var "x", Num 42),
                                                         Free(Var "x", Num 4)))))
                 |> Result.bind (getVar "result")

    Assert.Equal<Result<int,error>>(Error (MemoryNotAllocated 3), result)