module Tests

open System
open Assignment2

let rec dup (str : string) (n : int) =
    if n <= 0 then
        ""
    else
        str + dup str (n - 1)

let prefixSpace s (width : int) =
    let str = string s
    if width <= 0 || str.Length >= width then
        str
    else
        let diff = width - str.Length
        let prefix = String.init diff (fun _ -> " ")
        prefix + str

type Config() =
    class
        let mutable _continueOnFailure = false
        let mutable _verbose = false

        member this.continueOnFailure = _continueOnFailure
        member this.verbose = _verbose


        member this.setContinue b =
            _continueOnFailure <- b
            this

        member this.setVerbose b =
            _verbose <- b
            this
    end


type TestState(config: Config) =
    class
        let mutable _successes = 0u
        let mutable _lastTestNumber = 0u
        let _failedTests = []
        let mutable _region = ""
        let mutable _regionHasChangedSinceLastPrint = false
        member this.successes = _successes
        member this.lastTestNumber = _lastTestNumber
        member this.failedTests = _failedTests
        member this.currentRegion = _region
        member this.continueOnFailure = config.continueOnFailure
        member this.verbose = config.verbose
        member this.regionHasChanged = _regionHasChangedSinceLastPrint
        member this.incrementSuccesses() = _successes <- _successes + 1u
        member this.incrementTestNumber() = _lastTestNumber <- _lastTestNumber + 1u
        member this.addFailure(testNumber: uint) = _failedTests @ [ testNumber ]
        member this.setRegion(name : string) =
            _region <- name
            _regionHasChangedSinceLastPrint <- true

        member this.print msg =
            if this.verbose then
                if _regionHasChangedSinceLastPrint then
                    Console.WriteLine("--- " + _region + ":")
                    _regionHasChangedSinceLastPrint <- false
                Console.Write (string msg)
    end

let rec _parseArgs args config : Config =
    match args with
    | [] -> config
    | head :: tail ->
        match head with
        | "-c"
        | "--continue" -> _parseArgs tail (config.setContinue true)
        | "-v"
        | "--verbose" -> _parseArgs tail (config.setVerbose true)
        | _ -> _parseArgs tail config

let parseArgs args = _parseArgs args (Config())

// Called once before all tests.
let setupAll = ()

// Called before each test.
let setup = ()

// Called after each test.
let teardown = ()

// Called once after all tests.
let teardownAll = ()

let onSuccess (state : TestState) =
    state.print ": SUCCESS\n"
    state.incrementSuccesses ()
    teardown

let onFailure (msg : string) (state : TestState) =
    state.print ": FAILED\n"
    teardown

    if not state.continueOnFailure then
        if state.currentRegion <> "" then
            failwith("Assertion failure in region '" + state.currentRegion
                     + "' at test number " + string state.lastTestNumber
                     + ":\n" + msg)
        else
            failwith ("Assertion failure at test number " +
                   string state.lastTestNumber +
                   ":\n" + msg)

let assertIndividual expected actual (state: TestState) idx : bool =
    setup

    state.incrementTestNumber()
    state.print (prefixSpace state.lastTestNumber 4)
    if expected = actual then
        onSuccess state
        true
    else
        teardown
        false


let assertEqual expected actual (state: TestState) =
    setup

    state.incrementTestNumber ()
    state.print (prefixSpace state.lastTestNumber 4)

    if expected = actual then
        onSuccess state
    else
        onFailure ("Expected " + (string expected) + " but was " + (string actual) + ".") state

let assertThrows (func : 'a -> 'b) (arg : 'a) (state : TestState) =
    state.incrementTestNumber()
    state.print (prefixSpace state.lastTestNumber 4)

    try
        let _ = func arg
        onFailure "Expected to throw an exception, but did not." state
    with
    | _ -> onSuccess state

let assertThrows2 (func : 'a -> 'a -> 'b) (arg1 : 'a) (arg2 : 'a) (state : TestState) =
    state.incrementTestNumber()
    state.print (prefixSpace state.lastTestNumber 4)

    try
        let _ = func arg1 arg2
        onFailure "Expected to throw an exception, but did not." state
    with
    | _ -> onSuccess state


let rec assertAll (expected : 'a list) (input : 'a list) (f : 'a -> 'a) (state : TestState) =
    match (expected.Length, input.Length) with
    | 0,0 -> ()
    | a,b when a <> b ->
        failwith "The lengths of the 'expected' and 'actual' lists were different."
    | _,_ ->
        assertEqual expected.Head (f input.Head) state
        assertAll expected.Tail input.Tail f state

// Given a list of expected results and function arguments, a function, and the state,
let rec assertEach (lst : ('a * 'b) list) (f : 'b -> 'a) (state : TestState) =
    match lst with
    | [] -> ()
    | (expected, input)::tail ->
        let actual = f input
        if not (assertIndividual expected actual state 0) then
            onFailure ("Expected " + (string expected) + " but was " + (string actual) + ".\n"
                       + "Input was '" + string input + "'\n") state
        assertEach tail f state

[<EntryPoint>]
let main args =
    let config = parseArgs (List.ofArray args)
    let state = TestState config

    let test (expected: 'a) (actual: 'a) = assertEqual expected actual state
    let testThrows func arg = assertThrows func arg state
    let testThrows2 func arg1 arg2 = assertThrows2 func arg1 arg2 state
    let testAll (expected: 'a list) (actual: 'a list) (f:'a->'a) =
        assertAll expected actual f state
    let testEach (inputs:('a * 'b) list) (f : 'b -> 'a) =
        assertEach inputs f state

    setupAll

    state.setRegion "downto1"

    test [] (downto1 0)
    test [ 1 ] (downto1 1)
    test [ 2; 1 ] (downto1 2)
    test [ 3; 2; 1 ] (downto1 3)
    test [] (downto1 -5)
    test [] (downto1 -1)

    state.setRegion "downto2"

    test (downto1 0) (downto2 0)
    test (downto1 1) (downto2 1)
    test (downto1 2) (downto2 2)
    test (downto1 3) (downto2 3)
    test (downto1 -5) (downto2 -5)
    test (downto1 -1) (downto2 -1)

    state.setRegion "downto3"

    test (downto1 0) (downto3 0)
    test (downto1 1) (downto3 1)
    test (downto1 2) (downto3 2)
    test (downto1 3) (downto3 3)
    test (downto1 -5) (downto3 -5)
    test (downto1 -1) (downto3 -1)

    state.setRegion "removeOddIdx"

    test [] (removeOddIdx [])
    test [] (removeOddIdx [ "" ])
    test [ "b" ] (removeOddIdx [ "a"; "b" ])
    test [ 2; 4; 6; 8 ] (removeOddIdx [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ])

    state.setRegion "combinePair"

    test [] (combinePair [])
    test [] (combinePair [1])
    test [(1,2)] (combinePair [1;2])
    test [(1,2)] (combinePair [1;2;3])
    test [(1,2);(3,4)] (combinePair [1;2;3;4])
    test [(1,2);(3,4)] (combinePair [1;2;3;4;5])

    state.setRegion "explode1"

    test []            (explode1    "")
    test ['a']         (explode1   "a")
    test ['a';'b']     (explode1  "ab")
    test ['a';'b';'c'] (explode1 "abc")

    state.setRegion "explode2"

    test (explode1    "") (explode2    "")
    test (explode1   "a") (explode2   "a")
    test (explode1  "ab") (explode2  "ab")
    test (explode1 "abc") (explode2 "abc")

    state.setRegion "implode"

    test ""    (implode [])
    test "a"   (implode ['a'])
    test "ab"  (implode ['a';'b'])
    test "abc" (implode ['a';'b';'c'])

    state.setRegion "implodeRev"

    test "" (implodeRev [])
    test "a" (implodeRev ['a'])
    test "ba" (implodeRev ['a';'b'])
    test "cba" (implodeRev ['a';'b';'c'])

    state.setRegion "toUpper"

    test "" (toUpper "")
    test "A" (toUpper "a")
    test "AB" (toUpper "ab")
    test "ABC" (toUpper "abc")
    test "A" (toUpper "A")
    test "AB" (toUpper "AB")
    test "ABC" (toUpper "ABC")
    test "1" (toUpper "1")
    test "12" (toUpper "12")
    test "123" (toUpper "123")
    test "1A2" (toUpper "1a2")
    test "1A3" (toUpper "1A3")
    test " " (toUpper " ")
    test " A" (toUpper " a")
    test " A" (toUpper " A")
    test "A " (toUpper "a ")
    test "A " (toUpper "A ")
    test "2 A" (toUpper "2 a")
    test "2 A" (toUpper "2 A")
    test "A 2" (toUpper "a 2")
    test "A 2" (toUpper "A 2")
    test "2 A " (toUpper "2 a ")
    test "2 A " (toUpper "2 A ")
    test "A 2 " (toUpper "a 2 ")
    test "A 2 " (toUpper "A 2 ")
    test " 2 A " (toUpper " 2 a ")
    test " 2 A " (toUpper " 2 A ")
    test " A 2 " (toUpper " a 2 ")
    test " A 2 " (toUpper " A 2 ")

    state.setRegion "toUpper2"

    test (toUpper "") (toUpper2 "")
    test (toUpper "a") (toUpper2 "a")
    test (toUpper "ab") (toUpper2 "ab")
    test (toUpper "abc") (toUpper2 "abc")
    test (toUpper "A") (toUpper2 "A")
    test (toUpper "AB") (toUpper2 "AB")
    test (toUpper "ABC") (toUpper2 "ABC")
    test (toUpper "1") (toUpper2 "1")
    test (toUpper "12") (toUpper2 "12")
    test (toUpper "123") (toUpper2 "123")
    test (toUpper "1a2") (toUpper2 "1a2")
    test (toUpper "1A3") (toUpper2 "1A3")
    test (toUpper " ") (toUpper2 " ")
    test (toUpper " a") (toUpper2 " a")
    test (toUpper " A") (toUpper2 " A")
    test (toUpper "a ") (toUpper2 "a ")
    test (toUpper "A ") (toUpper2 "A ")
    test (toUpper "2 a") (toUpper2 "2 a")
    test (toUpper "2 A") (toUpper2 "2 A")
    test (toUpper "a 2") (toUpper2 "a 2")
    test (toUpper "A 2") (toUpper2 "A 2")
    test (toUpper "2 a ") (toUpper2 "2 a ")
    test (toUpper "2 A ") (toUpper2 "2 A ")
    test (toUpper "a 2 ") (toUpper2 "a 2 ")
    test (toUpper "A 2 ") (toUpper2 "A 2 ")
    test (toUpper " 2 a ") (toUpper2 " 2 a ")
    test (toUpper " 2 A ") (toUpper2 " 2 A ")
    test (toUpper " a 2 ") (toUpper2 " a 2 ")
    test (toUpper " A 2 ") (toUpper2 " A 2 ")

    state.setRegion "toUpper3"

    test (toUpper "") (toUpper3 "")
    test (toUpper "a") (toUpper3 "a")
    test (toUpper "ab") (toUpper3 "ab")
    test (toUpper "abc") (toUpper3 "abc")
    test (toUpper "A") (toUpper3 "A")
    test (toUpper "AB") (toUpper3 "AB")
    test (toUpper "ABC") (toUpper3 "ABC")
    test (toUpper "1") (toUpper3 "1")
    test (toUpper "12") (toUpper3 "12")
    test (toUpper "123") (toUpper3 "123")
    test (toUpper "1a2") (toUpper3 "1a2")
    test (toUpper "1A3") (toUpper3 "1A3")
    test (toUpper " ") (toUpper3 " ")
    test (toUpper " a") (toUpper3 " a")
    test (toUpper " A") (toUpper3 " A")
    test (toUpper "a ") (toUpper3 "a ")
    test (toUpper "A ") (toUpper3 "A ")
    test (toUpper "2 a") (toUpper3 "2 a")
    test (toUpper "2 A") (toUpper3 "2 A")
    test (toUpper "a 2") (toUpper3 "a 2")
    test (toUpper "A 2") (toUpper3 "A 2")
    test (toUpper "2 a ") (toUpper3 "2 a ")
    test (toUpper "2 A ") (toUpper3 "2 A ")
    test (toUpper "a 2 ") (toUpper3 "a 2 ")
    test (toUpper "A 2 ") (toUpper3 "A 2 ")
    test (toUpper " 2 a ") (toUpper3 " 2 a ")
    test (toUpper " 2 A ") (toUpper3 " 2 A ")
    test (toUpper " a 2 ") (toUpper3 " a 2 ")
    test (toUpper " A 2 ") (toUpper3 " A 2 ")

    state.setRegion "ack"

    test  1 (ack (0, 0))
    test  2 (ack (0, 1))
    test  3 (ack (0, 2))
    test  4 (ack (0, 3))
    test  0 (ack (0,-1))
    test -1 (ack (0,-2))
    testThrows ack (-1, 0)
    testThrows ack (-2, 0)
    testThrows ack (-3, 0)
    testThrows ack ( 1,-1)
    testThrows ack ( 1,-2)
    testThrows ack ( 1,-3)

    state.setRegion "reverse"

    test [] (reverse [])
    test [1] (reverse [1])
    test [2;1] (reverse [1;2])
    test [3;2;1] (reverse [1;2;3])

    state.setRegion "palindrome"

    let palindrome1Input = [
        true, ""
        true, "a"
        true, " "
        true, "1"
        true, "aa"
        false, "aA"
        true, "  "
        true, "11"
        false, "a1a1"
        true, "a1a1a"
        true, "a1A1a"
        false, "a1A1A"
        true, "a11a"
        false, "a12a"
        false, "a 2a"
        false, "a 2a."
        true, "aba"
        false, "a...ba"
        false, "a..b          a"
        false, "ab"
        false, "a 2b"
        false, "aaba"
    ]

    let palindrome2Input = [
        true, ""
        true, "a"
        true, " "
        true, "1"
        true, "aa"
        true, "aA"
        true, "  "
        true, "11"
        true, "a1a1"
        true, "a1A1"
        true, "a1a1a"
        true, "a1A1a"
        true, "a1A1A"
        true, "a11a"
        true, "a12a"
        true, "a12A"
        true, "a 2a"
        true, "a 2a."
        true, "a 2a.a"
        false, "a 2a.b"
        true, "aba"
        true, "ABA"
        true, "AbA"
        true, "a...ba"
        true, "a..b          a"
        false, "ab"
        false, "a 2b"
        false, "aaba"
    ]

    testEach palindrome1Input palindrome
    testEach palindrome2Input palindrome2
    testEach palindrome2Input palindrome3
    testEach palindrome2Input palindrome4
    testEach palindrome2Input palindrome5

    state.setRegion "complex numbers"

    test (complex(0,0)) (mkComplex 0 0)
    test (complex(0.25, 5.0)) (mkComplex 0.25 5.0)
    test (0.0,0.0) (complexToPair (complex(0.0, 0.0)))
    test (500.24,-34.2) (complexToPair (complex(500.24, -34.2)))

    let zero = complex(0,0)
    let c1 = complex(1,1)
    test zero (zero |+| zero)
    test zero (zero |-| zero)
    test zero (zero |*| zero)
    testThrows (~%%) zero
    testThrows2 (|/|) zero zero
    test zero (-zero)

    Console.WriteLine(
        string state.successes
        + "/"
        + string state.lastTestNumber
        + " tests completed successfully."
    )

    teardownAll

    0
