module _06.TestsMemory

open Interpreter.Eval
open Interpreter.Language
open Interpreter.State
open Xunit
open Interpreter.Memory

[<Fact>]
let TestEmpty () =
    let mem = (empty 0 |> alloc 0)

    match mem with
    | Ok v ->
        System.Console.WriteLine("[-] TestEmpty success: " + string v)
    | Error e ->
        System.Console.WriteLine("[+] TestEmpty error: " + string e)
    Assert.Equal(Error (NegativeMemoryAllocated 0), mem)

let create size : state =
    mkState size

[<Fact>]
let TestFreePtrLowerThanNextPtrPlusSizeGreaterThanNext () =
    let size = 20
    let st = create size
    let freed = Interpreter.State.free 10 30 st

    match freed with
    | Ok v ->
        System.Console.WriteLine("[-] TestFreePtrLower... success: " + string v)
    | Error e ->
        System.Console.WriteLine("[+] TestFreePtrLower... error: " + string e)
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

    match result with
    | Ok v ->
        System.Console.WriteLine("[+] AllocMemRead success: " + string v)
    | Error e ->
        System.Console.WriteLine("[-] AllocMemRead error: " + string e)
    Assert.Equal(Ok 0, result)

[<Fact>]
let Alloc2 () =
    let result =
        create 10
        |> stmntEval (Seq (Declare "x", Seq(Alloc ("x", Num 3),
                                        Seq(MemWrite(Var "x", Num 42), Seq(Declare "result",
                                                                           Assign("result", MemRead (Var "x")))))))
        |> Result.bind (getVar "result")

    match result with
    | Ok v ->
        System.Console.WriteLine("[+] Alloc2 success: " + string v)
    | Error e ->
        System.Console.WriteLine("[-] Alloc2 error: " + string e)
    Assert.Equal(Ok 42, result)

[<Fact>]
let Alloc3 () =
    let result = 10
                 |> mkState
                 |> stmntEval (Seq (Declare "x", Seq(Alloc ("x", Num 3), Seq(MemWrite(Var "x", Num
                        42), Seq(Declare "result", Assign("result", MemRead (Var "x" .+. Num 1)))))))
                 |> Result.bind (getVar "result")

    match result with
    | Ok v ->
        System.Console.WriteLine("[+] Alloc3 success: " + string v)
    | Error e ->
        System.Console.WriteLine("[-] Alloc3 error: " + string e)
    Assert.Equal(Ok 0, result)

[<Fact>]
let Alloc4 () =
    let result = 10 |> mkState
                 |> stmntEval (Seq (Declare "x", Seq(Alloc ("x", Num 3), Seq(MemWrite(Var "x", Num
                        42), Seq(Declare "result", Assign("result", MemRead (Var "x" .+. Num 3)))))))
                 |> Result.bind (getVar "result")

    match result with
    | Ok v ->
        System.Console.WriteLine("[-] Alloc4 success: " + string v)
    | Error e ->
        System.Console.WriteLine("[+] Alloc4 error: " + string e)

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

[<Fact>]
let Alloc8 () =
    let result =
        10 |>
        empty |>
        alloc 2 |>
        Result.bind (fst >> alloc 2) |>
        Result.bind (fst >> alloc 2) |>
        Result.bind (fst >> alloc 2) |>
        Result.bind (fst >> alloc 2) |>
        Result.bind (fst >> free 1 8) |>
        Result.bind (setMem 9 42) |>
        Result.bind (getMem 9)

    Assert.Equal<Result<int,error>>(Ok 42, result)