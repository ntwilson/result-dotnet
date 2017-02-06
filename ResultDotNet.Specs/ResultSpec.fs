namespace ResultDotNet.Specs

open NUnit.Framework

[<TestFixture>]
module ResultSpec =
    open Should
    open ResultDotNet
    open System

    [<Test>]
    let ``should have structural equality`` () =
        Success 5 |> shouldBe (Success 5)
        Failure "didn't work" |> shouldBe (Failure "didn't work")

        Success 5 |> shouldNotBe (Success 10)
        Failure "didn't work" |> shouldNotBe (Failure "hello world")

    

    [<Test>]
    let ``match member should return an appropriate value`` () =
        (Success 5).Match 
            (Func<int, string> (fun i -> i.ToString()))
            (Func<string, string> (id))
        |> shouldBe "5"
        
        (Failure "didn't work").Match 
            (Func<int, string> (fun i -> i.ToString()))
            (Func<string, string> (id))
        |> shouldBe "didn't work"

    [<Test>]
    let ``map member should work with non-results`` () =
        (Success 5).Map (Func<int, int> ((+) 3))
        |> shouldBe (Success 8)

        (Failure "didn't work").Map (Func<int, int> ((+) 3))
        |> shouldBe (Result<int, string>.Failure "didn't work")

    [<Test>]
    let ``bind member should work with result funcs`` () =
        (Success 5).Bind (Func<int, Result<int,string>> (fun i -> Success (i + 3)))
        |> shouldBe (Result<int,string>.Success 8)

        (Failure "didn't work").Bind (Func<int, Result<int, string>> (fun i -> Success (i + 3)))
        |> shouldBe (Result<int, string>.Failure "didn't work")

    [<Test>]
    let ``static members behave the same as the members`` () =
        Success 5 |> Result.Map (Func<int, int> ((+) 3))
        |> shouldBe (Success 8)

        Failure "didn't work" |> Result.Map (Func<int, int> ((+) 3))
        |> shouldBe (Result<int, string>.Failure "didn't work")
    
        Success 5 |> Result.Bind (Func<int, Result<int,string>> (fun i -> Success (i + 3)))
        |> shouldBe (Result<int,string>.Success 8)

        Failure "didn't work" |> Result.Bind (Func<int, Result<int, string>> (fun i -> Success (i + 3)))
        |> shouldBe (Result<int, string>.Failure "didn't work")
