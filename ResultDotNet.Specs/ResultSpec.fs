namespace ResultDotNet.Specs

open NUnit.Framework

[<TestFixture>]
module ResultSpec =
    open Should
    open ResultDotNet
    open ResultDotNet.FSharp
    open System

    [<Test>]
    let ``should have structural equality`` () =
        Success 5 |> shouldBe (Success 5)
        Failure "didn't work" |> shouldBe (Failure "didn't work")

        Success 5 |> shouldNotBe (Success 10)
        Failure "didn't work" |> shouldNotBe (Failure "hello world")

    [<Test>]
    let ``static functions behave the same as the members`` () =
        Success 5 |> Result.map ((+) 3)
        |> shouldBe (Success 8)

        Failure "didn't work" |> Result.map ((+) 3)
        |> shouldBe (Result<int, string>.Failure "didn't work")
    
        Success 5 |> Result.bind (fun i -> Success (i + 3))
        |> shouldBe (Success 8)

        Failure "didn't work" |> Result.bind (fun i -> Success (i + 3))
        |> shouldBe (Result<int, string>.Failure "didn't work")

    [<Test>]
    let ``can map two results`` () =
        Result.map2 (fun i j -> (float i) + j)
            (Success 3)
            (Success 5.)
        |> shouldBe (Success 8.)

        Result.map2 (fun i j -> (float i) + j)
            (Failure "didn't work")
            (Success 5.)
        |> shouldBe (Result<float, string>.Failure "didn't work")
        
        Result.map2 (fun i j -> (float i) + j)
            (Success 3)
            (Failure "didn't work")
        |> shouldBe (Result<float, string>.Failure "didn't work")

    [<Test>]
    let ``can bind two results`` () =
        Result.bind2 (fun i j -> Success ((float i) + j))
            (Success 3)
            (Success 5.)
        |> shouldBe (Success 8.)

        Result.bind2 (fun i j -> Success ((float i) + j))
            (Failure "didn't work")
            (Success 5.)
        |> shouldBe (Result<float, string>.Failure "didn't work")
        
        Result.bind2 (fun i j -> Success ((float i) + j))
            (Success 3)
            (Failure "didn't work")
        |> shouldBe (Result<float, string>.Failure "didn't work")

    [<Test>]
    let ``can map three results`` () =
        Result.map3 (fun i j k -> i.ToString() + j.ToString() + k)
            (Success 13)
            (Success 37.)
            (Success "code")
        |> shouldBe (Success "1337code")

        Result.map3 (fun i j k -> i.ToString() + j.ToString() + k)
            (Failure "didn't work")
            (Success 37.)
            (Success "code")
        |> shouldBe (Result<string, string>.Failure "didn't work")
        
        Result.map3 (fun i j k -> i.ToString() + j.ToString() + k)
            (Success 13)
            (Success 37.)
            (Failure "didn't work")
        |> shouldBe (Result<string, string>.Failure "didn't work")
    
    [<Test>]
    let ``can bind three results`` () =
        Result.bind3 (fun i j k -> Success (i.ToString() + j.ToString() + k))
            (Success 13)
            (Success 37.)
            (Success "code")
        |> shouldBe (Success "1337code")

        Result.bind3 (fun i j k -> Success (i.ToString() + j.ToString() + k))
            (Failure "didn't work")
            (Success 37.)
            (Success "code")
        |> shouldBe (Result<string, string>.Failure "didn't work")
        
        Result.bind3 (fun i j k -> Success (i.ToString() + j.ToString() + k))
            (Success 13)
            (Success 37.)
            (Failure "didn't work")
        |> shouldBe (Result<string, string>.Failure "didn't work")
    
    [<Test>]
    let ``can map four results`` () =
        Result.map4 (fun i j k l -> i.ToString() + j.ToString() + k + l)
            (Success 13)
            (Success 37.)
            (Success "co")
            (Success "de")
        |> shouldBe (Success "1337code")

        Result.map4 (fun i j k l -> i.ToString() + j.ToString() + k + l)
            (Failure "didn't work")
            (Success 37.)
            (Success "co")
            (Success "de")
        |> shouldBe (Result<string, string>.Failure "didn't work")
        
        Result.map4 (fun i j k l -> i.ToString() + j.ToString() + k + l)
            (Success 13)
            (Success 37.)
            (Success "co")
            (Failure "didn't work")
        |> shouldBe (Result<string, string>.Failure "didn't work")
    
    [<Test>]
    let ``can bind four results`` () =
        Result.bind4 (fun i j k l -> Success (i.ToString() + j.ToString() + k + l))
            (Success 13)
            (Success 37.)
            (Success "co")
            (Success "de")
        |> shouldBe (Success "1337code")

        Result.bind4 (fun i j k l -> Success (i.ToString() + j.ToString() + k + l))
            (Failure "didn't work")
            (Success 37.)
            (Success "co")
            (Success "de")
        |> shouldBe (Result<string, string>.Failure "didn't work")
        
        Result.bind4 (fun i j k l -> Success (i.ToString() + j.ToString() + k + l))
            (Success 13)
            (Success 37.)
            (Success "co")
            (Failure "didn't work")
        |> shouldBe (Result<string, string>.Failure "didn't work")
    