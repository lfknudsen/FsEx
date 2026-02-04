module a02.Assertions

open State

let failureMsg expected actual input =
    ("Expected "
     + (string expected)
     + " but was "
     + (string actual)
     + ".\n"
     + "Input was '"
     + string input
     + "'\n")

/// Called by assertEach as an alternative to assertEqual.
/// It differs from assertEqual in that it returns a boolean indicating success
/// instead of calling onSuccess and onFailure.
let assertIndividual expected actual (state: TestState) : bool =
    state.nextTest ()
    expected = actual

/// Applies 'f' to 'input', and then asserts that the result is equal to
/// 'expected'.
let assertApply expected f input (state: TestState) =
    state.nextTest ()
    let actual = f input

    if expected = actual then
        state.onSuccess ()
    else
        state.onFailure (failureMsg expected actual input)

/// <summary>
/// Asserts that 'expected' is equal to 'actual'.
/// </summary>
/// <remarks>
/// Has side effects; it will increment counters in TestState and call
/// <see cref="T:TestState.print"/>.<br/>
/// If state.shouldContinueOnFailure is false (the default) and
/// 'expected' is not equal to 'actual', this function will throw an
/// exception. This behaviour can be overridden by launching the application
/// with --continue/-c.
/// </remarks>
let assertEqual expected actual (state: TestState) =
    state.nextTest ()

    if expected = actual then
        state.onSuccess ()
    else
        state.onFailure ("Expected " + (string expected) + " but was " + (string actual) + ".")

/// Asserts that applying the function 'func' to 'arg' will
/// cause an exception to be thrown.
let assertThrows (func: 'a -> 'b) (arg: 'a) (state: TestState) =
    state.nextTest ()

    try
        let _ = func arg
        state.onFailure "Expected to throw an exception, but did not."
    with _ ->
        state.onSuccess ()

/// Asserts that applying function 'func' to the two (non-tuple) arguments
/// 'arg1' and 'arg2' causes an exception to be thrown.
let assertThrows2 (func: 'a -> 'a -> 'b) (arg1: 'a) (arg2: 'a) (state: TestState) =
    state.nextTest ()

    try
        let _ = func arg1 arg2
        state.onFailure "Expected to throw an exception, but did not."
    with _ ->
        state.onSuccess ()

/// Given a list of expected outputs, a list of matching incomes, and a
/// function to apply to each one of these inputs, calls assertIndividual for each
/// element.
/// If an exception occurs to due to an unexpected result, the message will also
/// include the input, as well as the expected and actual values.
/// A convenience function for when testing the same function on many different
/// inputs.
let rec assertAll (expected: 'a list) (input: 'a list) (f: 'a -> 'a) (state: TestState) =
    match (expected.Length, input.Length) with
    | 0, 0 -> ()
    | a, b when a <> b ->
        failwith "The lengths of the 'expected' and 'actual' lists were different."
    | _, _ ->
        let actual = f input.Head

        if assertIndividual expected.Head actual state then
            state.onSuccess ()
        else
            state.onFailure (failureMsg expected.Head actual input.Head)

        assertAll expected.Tail input.Tail f state

/// Very similar to assertAll, but receives the expected and input values as a
/// list of matching pairs; (expected, input).
/// As opposed to assertEqual, the assertion failure exception message will
/// include the input in addition to expected and actual values.
let rec assertEach (lst: ('a * 'b) list) (f: 'b -> 'a) (state: TestState) =
    match lst with
    | [] -> ()
    | (expected, input) :: tail ->
        let actual = f input

        if assertIndividual expected actual state then
            state.onSuccess ()
        else
            state.onFailure (failureMsg expected actual input)

        assertEach tail f state
