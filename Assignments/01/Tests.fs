(*
    This programme is as much a test suite for the first mandatory assignment
    as it was me experimenting a bit with F#.
    Targets .NET 8.0 because I couldn't remember which the course recommended,
    but I felt quite confident it wasn't the current version. You can probably
    change it if you've got an even older version installed.

    To use this with your Assignment1.fs, put it into the same directory
    as this file.

    This file/project defines a console application that you would run in the
    same way as any other, e.g. with 'dotnet run'.
    'dotnet test' won't do anything.
    A run configuration file is included for JetBrains' IDEs.

    Supports the command-line argument
    --continue or -c
        Makes the programme continue running if a test fails.
        If this is omitted, a failed test will immediately throw an exception.

 *)
module Tests

open System
open Assignment1

let AssertEqual (expected : 'a) (actual : 'a)
        (successes : uint, lastTestNumber : uint, continueOnFailure : bool) =
    let currentTestNumber = lastTestNumber + 1u
    Console.Write(string currentTestNumber)
    if expected = actual then
        Console.Write(": SUCCESS\n")
        (successes + 1u, currentTestNumber, continueOnFailure)
    else
        Console.Write(": FAILED\n")
        if not continueOnFailure then
            failwith ("Expected " + (string expected) + " but was " + (string actual) + ".")
        else
            (successes, currentTestNumber, continueOnFailure)

let rec shouldContinueOnFailure args =
    match args with
    | [] -> false
    | ["-c"]
    | ["--continue"] -> true
    | head::tail ->
        match head with
        | "-c"
        | "--continue" -> true
        | _ -> shouldContinueOnFailure tail

[<EntryPoint>]
let main args =
    let continueOnFailure = shouldContinueOnFailure (List.ofArray args)
    // (# of successes, last test number, whether to continue on failure)
    let mutable state = (0u, 0u, continueOnFailure)
    // sqr
    state <- AssertEqual 0 (sqr 0) state
    state <- AssertEqual 1 (sqr 1) state
    state <- AssertEqual 4 (sqr 2) state
    state <- AssertEqual 1 (sqr -1) state

    // pow
    state <- AssertEqual 1.0 (pow 0 0) state
    state <- AssertEqual 0.0 (pow 0 1) state
    state <- AssertEqual 0.0 (pow 0 2) state
    state <- AssertEqual 0.0 (pow 0 3) state

    state <- AssertEqual 1.0 (pow 1 0) state
    state <- AssertEqual 1.0 (pow 1 1) state
    state <- AssertEqual 1.0 (pow 1 2) state
    state <- AssertEqual 1.0 (pow 1 3) state

    state <- AssertEqual 1.0 (pow 2 0) state
    state <- AssertEqual 2.0 (pow 2 1) state
    state <- AssertEqual 4.0 (pow 2 2) state
    state <- AssertEqual 8.0 (pow 2 3) state

    // fib
    state <- AssertEqual  0 (fib 0) state
    state <- AssertEqual  1 (fib 1) state
    state <- AssertEqual  1 (fib 2) state
    state <- AssertEqual  2 (fib 3) state
    state <- AssertEqual  3 (fib 4) state
    state <- AssertEqual  5 (fib 5) state
    state <- AssertEqual  8 (fib 6) state
    state <- AssertEqual 13 (fib 7) state

    // sum
    state <- AssertEqual  0 (sum 0) state
    state <- AssertEqual  1 (sum 1) state
    state <- AssertEqual  3 (sum 2) state
    state <- AssertEqual  6 (sum 3) state
    state <- AssertEqual 10 (sum 4) state
    state <- AssertEqual 15 (sum 5) state
    state <- AssertEqual 21 (sum 6) state
    state <- AssertEqual 28 (sum 7) state

    // dup
    state <- AssertEqual "" (dup "") state
    state <- AssertEqual "aa" (dup "a") state
    state <- AssertEqual "GG" (dup "G") state
    state <- AssertEqual "abcdEfghijklmnOpåabcdEfghijklmnOpå" (dup "abcdEfghijklmnOpå") state

    // dupn
    state <- AssertEqual "" (dupn "" 0) state
    state <- AssertEqual "" (dupn "abcdEfghijklmnOpå" 0) state
    state <- AssertEqual "" (dupn "a" 0) state
    state <- AssertEqual "" (dupn "A" 0) state
    state <- AssertEqual "" (dupn "\0" 0) state
    state <- AssertEqual "" (dupn "å" 0) state
    state <- AssertEqual "" (dupn "Å" 0) state

    state <- AssertEqual "" (dupn "" 1) state
    state <- AssertEqual "abcdEfghijklmnOpå" (dupn "abcdEfghijklmnOpå" 1) state
    state <- AssertEqual "a" (dupn "a" 1) state
    state <- AssertEqual "A" (dupn "A" 1) state
    state <- AssertEqual "\0" (dupn "\0" 1) state
    state <- AssertEqual "å" (dupn "å" 1) state
    state <- AssertEqual "Å" (dupn "Å" 1) state

    state <- AssertEqual (dup "") (dupn "" 2) state
    state <- AssertEqual (dup "abcdEfghijklmnOpå") (dupn "abcdEfghijklmnOpå" 2) state
    state <- AssertEqual (dup "a") (dupn "a" 2) state
    state <- AssertEqual (dup "A") (dupn "A" 2) state
    state <- AssertEqual (dup "\0") (dupn "\0" 2) state
    state <- AssertEqual (dup "å") (dupn "å" 2) state
    state <- AssertEqual (dup "Å") (dupn "Å" 2) state

    state <- AssertEqual "" (dupn "" 3) state
    state <- AssertEqual "abcdEfghijklmnOpåabcdEfghijklmnOpåabcdEfghijklmnOpå"
            (dupn "abcdEfghijklmnOpå" 3) state
    state <- AssertEqual "aaa" (dupn "a" 3) state
    state <- AssertEqual "AAA" (dupn "A" 3) state
    state <- AssertEqual "\0\0\0" (dupn "\0" 3) state
    state <- AssertEqual "ååå" (dupn "å" 3) state
    state <- AssertEqual "ÅÅÅ" (dupn "Å" 3) state

    state <- AssertEqual "" (dupn "" 4) state
    state <- AssertEqual "abcdEfghijklmnOpåabcdEfghijklmnOpåabcdEfghijklmnOpåabcdEfghijklmnOpå"
            (dupn "abcdEfghijklmnOpå" 4) state
    state <- AssertEqual "aaaa" (dupn "a" 4) state
    state <- AssertEqual "AAAA" (dupn "A" 4) state
    state <- AssertEqual "\0\0\0\0" (dupn "\0" 4) state
    state <- AssertEqual "åååå" (dupn "å" 4) state
    state <- AssertEqual "ÅÅÅÅ" (dupn "Å" 4) state

    // bin
    state <- AssertEqual 1 (bin(0,0)) state
    state <- AssertEqual 1 (bin(1,1)) state
    state <- AssertEqual 1 (bin(2,2)) state
    state <- AssertEqual 1 (bin(3,3)) state
    state <- AssertEqual 1 (bin(Int32.MaxValue, Int32.MaxValue)) state
    state <- AssertEqual 1 (bin(-1,-1)) state
    state <- AssertEqual 1 (bin(-2,-2)) state
    state <- AssertEqual 1 (bin(-3,-3)) state
    state <- AssertEqual 1 (bin(Int32.MinValue, Int32.MinValue)) state

    state <- AssertEqual 1 (bin(1,0)) state
    state <- AssertEqual 1 (bin(2,0)) state
    state <- AssertEqual 1 (bin(3,0)) state
    state <- AssertEqual 1 (bin(Int32.MaxValue, 0)) state
    state <- AssertEqual 1 (bin(-1,0)) state
    state <- AssertEqual 1 (bin(-2,0)) state
    state <- AssertEqual 1 (bin(-3,0)) state
    state <- AssertEqual 1 (bin(Int32.MinValue, 0)) state

    state <- AssertEqual 2 (bin(2,1)) state
    state <- AssertEqual 3 (bin(3,1)) state
    state <- AssertEqual 3 (bin(3,2)) state
    state <- AssertEqual 4 (bin(4,1)) state
    state <- AssertEqual 6 (bin(4,2)) state
    state <- AssertEqual 4 (bin(4,3)) state

    // timediff
    state <- AssertEqual   0 (timediff (0,0) (0,0)) state
    state <- AssertEqual   0 (timediff (19,23) (19,23)) state
    state <- AssertEqual  84 (timediff (0,0) (1,24)) state
    state <- AssertEqual -84 (timediff (1,24) (0,0)) state

    // minutes
    state <- AssertEqual 0 (minutes (0,0)) state
    state <- AssertEqual 45 (minutes (0,45)) state
    state <- AssertEqual 465 (minutes (7,45)) state

    // curry
    state <- AssertEqual (minutes (7,45)) (curry minutes 7 45) state
    state <- AssertEqual (dupn "-" 6) (uncurry dupn ("-", 6)) state

    let successes, testCount, _ = state
    Console.WriteLine(string successes + "/" + string testCount + " tests completed successfully.");
    0
