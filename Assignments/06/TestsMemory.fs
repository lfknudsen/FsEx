module _06.TestsMemory

open System.Collections.Generic
open Xunit
open Interpreter.Memory

[<Fact>]
let EmptyMemory () =
    let mem = empty 10
    Assert.Equal(0, mem.next)
    Assert.Equal(0, mem.heap.Count)

[<Fact>]
let TestAlloc () =
    let allocation = empty 10 |> alloc 20
    Assert.True(allocation.IsSome)

    let mem, next = allocation.Value
    Assert.Equal(20, mem.next)
    Assert.Equal(mem.next, next)
    Assert.Equal(20, mem.heap.Count)

    Assert.All(mem.heap, (fun (kv : KeyValuePair<int,int>) -> Assert.Equal(kv.Value, 0)))

[<Theory>]
[<InlineData(-3)>]
[<InlineData(0)>]
[<InlineData(-1)>]
let FailToAlloc (size : int) =
    let allocation = empty 10 |> alloc size
    Assert.Equal(None, allocation)