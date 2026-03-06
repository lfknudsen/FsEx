module Tests

open Interpreter.State
open System
open Xunit

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
    Assert.Equal<int option>(None, () |> mkState |> getVar "x")
    Assert.Equal<int option>(Some 0, () |> mkState |> declare "x" |> Option.bind (getVar "x"))

    Assert.Equal<int option>(
        Some 42,
        ()
        |> mkState
        |> declare "x"
        |> Option.bind (setVar "x" 42)
        |> Option.bind (getVar "x")
    )

    Assert.Equal<int option>(
        None,
        ()
        |> mkState
        |> declare "1x"
        |> Option.bind (setVar "1x" 42)
        |> Option.bind (getVar "1x")
    )