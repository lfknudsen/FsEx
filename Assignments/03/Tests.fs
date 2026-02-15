// Note:
// Unlike 01 and 02, these are executed with 'dotnet test'
//
module Tests

open Assignment3
open System.Text.RegularExpressions
open Xunit

[<Fact>]
let ``Add5`` () =
    Assert.Equal(5, add5 0)
    Assert.Equal(4, add5 -1)
    Assert.Equal(6, add5 1)
    Assert.Equal(7, add5 2)

[<Fact>]
let ``Mul3`` () =
    Assert.Equal(3, mul3 1)
    Assert.Equal(9, mul3 3)
    Assert.Equal(-12, mul3 -4)

[<Fact>]
let ``Add5Mul3`` () =
    Assert.Equal(30, add5mul3 5)
    Assert.Equal(0, add5mul3 -5)
    Assert.Equal(15, add5mul3 0)

[<Fact>]
let ``Add5Mul3_2`` () =
    Assert.Equal(add5mul3 5, add5mul3_2 5)
    Assert.Equal(add5mul3 -5, add5mul3_2 -5)
    Assert.Equal(add5mul3 0, add5mul3_2 0)

[<Fact>]
let ``Add5_2`` () =
    Assert.Equal(15, add5_2 add5 5)
    Assert.Equal(5, add5_2 add5 -5)

[<Fact>]
let ``Mul3_2`` () =
    Assert.Equal(30, mul3_2 add5 5)
    Assert.Equal(0, mul3_2 add5 -5)

[<Theory>]
[<InlineData(1, 0)>]
[<InlineData(1, 1)>]
[<InlineData(2, 2)>]
[<InlineData(6, 3)>]
[<InlineData(24, 4)>]
let ``fac_downto4`` (exp : int, input : int) =
    Assert.Equal(exp, fac input)

[<Fact>]
let ``range_downto4`` () =
    Assert.Equal<int list>([], range add5 0)
    Assert.Equal<int list>([], range add5 -5)
    Assert.Equal<int list>([6], range add5 1)
    Assert.Equal<int list>([6; 7], range add5 2)
    Assert.Equal<int list>([6; 7; 8], range add5 3)
    Assert.Equal<int list>([6; 7; 8; 9], range add5 4)
    Assert.Equal<int list>([3; 6; 9; 12], range mul3 4)

[<Fact>]
let ``double_1`` () =
    Assert.Equal<int list>([], double [])
    Assert.Equal<int list>([2], double [1])
    Assert.Equal<int list>([12], double [6])
    Assert.Equal<int list>([-2], double [-1])
    Assert.Equal<int list>([0], double [0])
    Assert.Equal<int list>([2;4], double [1;2])
    Assert.Equal<int list>([12;14;16], double [6;7;8])
    Assert.Equal<int list>([-2;-4], double [-1;-2])
    Assert.Equal<int list>([0;0;-2], double [0;0;-1])

[<Fact>]
let ``double_2`` () =
    Assert.Equal<int list>(double [], double_2 [])
    Assert.Equal<int list>(double [1], double_2 [1])
    Assert.Equal<int list>(double [6], double_2 [6])
    Assert.Equal<int list>(double [-1], double_2 [-1])
    Assert.Equal<int list>(double [0], double_2 [0])
    Assert.Equal<int list>(double [1;2], double_2 [1;2])
    Assert.Equal<int list>(double [6;7;8], double_2 [6;7;8])
    Assert.Equal<int list>(double [-1;-2], double_2 [-1;-2])
    Assert.Equal<int list>(double [0;0;-1], double_2 [0;0;-1])

[<Fact>]
let ``str_len_1`` () =
    Assert.Equal<int list>([], stringLength [])
    Assert.Equal<int list>([0], stringLength [""])
    Assert.Equal<int list>([5], stringLength ["Hello"])
    Assert.Equal<int list>([5; 2; 5; 1], stringLength ["Hello"; ", "; "World"; "!"])

    Assert.Equal<int list>(stringLength [], stringLength_2 [])
    Assert.Equal<int list>(stringLength [""], stringLength_2 [""])
    Assert.Equal<int list>(stringLength ["Hello"], stringLength_2 ["Hello"])
    Assert.Equal<int list>(stringLength ["Hello"; ", "; "World"; "!"],
                 stringLength_2 ["Hello"; ", "; "World"; "!"])

[<Fact>]
let ``keep_even`` () =
    Assert.Equal<int list>([], keepEven [])
    Assert.Equal<int list>([], keepEven [1])
    Assert.Equal<int list>([2], keepEven [2])
    Assert.Equal<int list>([2], keepEven [1;2])
    Assert.Equal<int list>([2], keepEven [2;3])
    Assert.Equal<int list>([2], keepEven [1;2;3])
    Assert.Equal<int list>([2;4;6;8], keepEven [1;2;3;4;5;7;11;6;8])

    Assert.Equal<int list>(keepEven [], keepEven_2 [])
    Assert.Equal<int list>(keepEven [], keepEven_2 [1])
    Assert.Equal<int list>(keepEven [2], keepEven_2 [2])
    Assert.Equal<int list>(keepEven [1;2], keepEven_2 [1;2])
    Assert.Equal<int list>(keepEven [2;3], keepEven_2 [2;3])
    Assert.Equal<int list>(keepEven [1;2;3], keepEven_2 [1;2;3])
    Assert.Equal<int list>(keepEven [1;2;3;4;5;7;11;6;8],
                           keepEven_2 [1;2;3;4;5;7;11;6;8])

[<Fact>]
let ``keepLength_gt5`` () =
    let inputs = [
                  ([], [])
                  ([], [""])
                  [], ["";"";"";""]
                  [], ["a"]
                  ["looooooooong"], ["looooooooong"]
                  ["looooooooong"], ["a"; "looooooooong"]
                  [], ["four"]
                  [], ["fives"]
                  [], ["fives"; "fives"]
                  ["sixchs"], ["sixchs"]
                  ["sixchs"; "sixchs"], ["sixchs"; "sixchs"]
                  ["sixchs"; "sixchs"], ["sixchs"; "a"; "sixchs"]
                  ["looooooong"], ["looooooong"; "a"]
                  ]

    for i in inputs do
        Assert.Equal<string list>(fst i, keepLengthGT5 (snd i))
        Assert.Equal<string list>(keepLengthGT5 (snd i), keepLengthGT5_2 (snd i))

[<Fact>]
let ``sumPos`` () =
    let inputs = [
        0,[]
        0,[0]
        0,[-1]
        0,[-2]
        1,[1]
        2,[2]
        1,[-1;1]
        1,[1;-1]
        1,[-1;1;-1]
    ]

    for i in inputs do
        let actual = sumPositive (snd i)
        Assert.Equal(fst i, actual)
        Assert.Equal(actual, sumPositive_2 (snd i))
        Assert.Equal(actual, sumPositive_3 (snd i))

[<Fact>]
let ``Add5Mul3_3`` () =
    Assert.Equal(15, add5mul3_3 mul3 0)
    Assert.Equal(24, add5mul3_3 mul3 1)
    Assert.Equal(33, add5mul3_3 mul3 2)

[<Fact>]
let ``MergeFuns`` () =
    Assert.Equal(42, mergeFuns [add5; mul3] 9)
    Assert.Equal(32, mergeFuns [mul3; add5] 9)
    Assert.Equal(27, mergeFuns [mul3] 9)
    Assert.Equal(9, mergeFuns [] 9)

[<Fact>]
let ``keepOddIdx`` () =
    Assert.Equal(0, (removeOddIdx []).Length)
    Assert.Equal<int list>([1], removeOddIdx [1])
    Assert.Equal<int list>([1], removeOddIdx [1;2])
    Assert.Equal<int list>([2], removeOddIdx [2])
    Assert.Equal<int list>([2], removeOddIdx [2;1])
    Assert.Equal<int list>([2;4], removeOddIdx [2;1;4])

[<Theory>]
[<InlineData(1, 0)>]
[<InlineData(1, 1)>]
[<InlineData(2, 2)>]
[<InlineData(6, 3)>]
[<InlineData(24, 4)>]
let ``fac2`` (exp : int, input : int) =
    Assert.Equal(exp, fac_2 input)

[<Fact>]
let ``Weird`` () =
    let input = "I feel pretty, oh so pretty, I feel pretty and witty and bright, and I pity any girl who isn't me tonight";
    let split = Regex.Split(input, "\s")
    Assert.Equal("42246442", split |> Array.toList |> weird)
    Assert.Equal("0", weird [""])
    Assert.Equal("", weird [])

[<Fact>]
let ``Insert`` () =
    Assert.Equal<int list>([[1]], insert 1 [])
    Assert.Equal<int list>([[0;1];[1;0]], insert 0 [1])
    Assert.Equal<int list>([[0;1;2];[1;0;2];[1;2;0]], insert 0 [1;2])
    Assert.Equal<int list>([[0;1;2;3];[1;0;2;3];[1;2;0;3];[1;2;3;0]], insert 0 [1;2;3])

[<Fact>]
let ``Permutations`` () =
    Assert.Equal<int list>([[]], permutations [])

    let act2 = permutations [1;2] |> List.sort
    Assert.Equal<int list list>([[1]], permutations [1])
    Assert.Equal<int list list>(List.sort [[1;2];[2;1]], act2)
    Assert.Equal<int list list>(List.sort [[1;2;3];[1;3;2];[2;1;3];[2;3;1];[3;1;2];[3;2;1]]
                                , permutations [1;2;3] |> List.sort)
    Assert.Equal<int list list>(List.sort [[1;2;3;4];[1;3;2;4];[2;1;3;4];[2;3;1;4];[3;1;2;4];[3;2;1;4]
                                           [1;2;4;3];[1;3;4;2];[2;1;4;3];[2;3;4;1];[3;1;4;2];[3;2;4;1]
                                           [1;4;2;3];[1;4;3;2];[2;4;1;3];[2;4;3;1];[3;4;1;2];[3;4;2;1]
                                           [4;1;2;3];[4;1;3;2];[4;2;1;3];[4;2;3;1];[4;3;1;2];[4;3;2;1]],
                                List.sort <| permutations [1;2;3;4])

    let expected = List.sort [["I"; "feel"; "pretty"]; ["feel"; "I"; "pretty"]; ["feel"; "pretty"; "I"];
                ["I"; "pretty"; "feel"]; ["pretty"; "I"; "feel"]; ["pretty"; "feel"; "I"]]
    let actual = permutations ["I"; "feel"; "pretty"] |> List.sort
    Assert.Equal(expected.Length, actual.Length)
    for str in actual do
        Assert.Equal(3, str.Length)
    Assert.Equal<string list list>(expected, actual)
