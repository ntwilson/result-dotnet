namespace ResultDotNet.Specs

open NUnit.Framework

[<TestFixture>]
module ResultSpec =
    open Should
    open ResultDotNet

    [<Test>]
    let works () =
        Success 5
        |> shouldBe (Success 5)
