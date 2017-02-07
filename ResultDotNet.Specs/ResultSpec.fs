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

    [<Test>]
    let ``can create Results with static functions instead of the normal constructors`` ()  =
        Result.Success 5 |> shouldBe (Success 5)
        Result.Failure "didn't work" |> shouldBe (Failure "didn't work")

    [<Test>]
    let ``can map two results`` () =
        Result.Map2 (Func<int, float, float> (fun i j -> (float i) + j))
            (Success 3)
            (Success 5.)
        |> shouldBe (Success 8.)

        Result.Map2 (Func<int, float, float> (fun i j -> (float i) + j))
            (Failure "didn't work")
            (Success 5.)
        |> shouldBe (Result<float, string>.Failure "didn't work")
        
        Result.Map2 (Func<int, float, float> (fun i j -> (float i) + j))
            (Success 3)
            (Failure "didn't work")
        |> shouldBe (Result<float, string>.Failure "didn't work")

    [<Test>]
    let ``can bind two results`` () =
        Result.Bind2 (Func<int, float, Result<float, string>> (fun i j -> Success ((float i) + j)))
            (Success 3)
            (Success 5.)
        |> shouldBe (Result<float, string>.Success 8.)

        Result.Bind2 (Func<int, float, Result<float, string>> (fun i j -> Success ((float i) + j)))
            (Failure "didn't work")
            (Success 5.)
        |> shouldBe (Result<float, string>.Failure "didn't work")
        
        Result.Bind2 (Func<int, float, Result<float, string>> (fun i j -> Success ((float i) + j)))
            (Success 3)
            (Failure "didn't work")
        |> shouldBe (Result<float, string>.Failure "didn't work")

    [<Test>]
    let ``can map three results`` () =
        Result.Map3 (Func<int, float, string, string> (fun i j k -> i.ToString() + j.ToString() + k))
            (Success 13)
            (Success 37.)
            (Success "code")
        |> shouldBe (Success "1337code")

        Result.Map3 (Func<int, float, string, string> (fun i j k -> i.ToString() + j.ToString() + k))
            (Failure "didn't work")
            (Success 37.)
            (Success "code")
        |> shouldBe (Result<string, string>.Failure "didn't work")
        
        Result.Map3 (Func<int, float, string, string> (fun i j k -> i.ToString() + j.ToString() + k))
            (Success 13)
            (Success 37.)
            (Failure "didn't work")
        |> shouldBe (Result<string, string>.Failure "didn't work")
    
    [<Test>]
    let ``can bind three results`` () =
        Result.Bind3 
            (Func<int, float, string, Result<string,string>> (fun i j k -> 
                Success (i.ToString() + j.ToString() + k)))
            (Success 13)
            (Success 37.)
            (Success "code")
        |> shouldBe (Result<string, string>.Success "1337code")

        Result.Bind3 
            (Func<int, float, string, Result<string,string>> (fun i j k -> 
                Success (i.ToString() + j.ToString() + k)))
            (Failure "didn't work")
            (Success 37.)
            (Success "code")
        |> shouldBe (Result<string, string>.Failure "didn't work")
        
        Result.Bind3 
            (Func<int, float, string, Result<string,string>> (fun i j k -> 
                Success (i.ToString() + j.ToString() + k)))
            (Success 13)
            (Success 37.)
            (Failure "didn't work")
        |> shouldBe (Result<string, string>.Failure "didn't work")
    
    [<Test>]
    let ``can map four results`` () =
        Result.Map4 
            (Func<int, float, string, string, string> (fun i j k l -> 
                i.ToString() + j.ToString() + k + l))
            (Success 13)
            (Success 37.)
            (Success "co")
            (Success "de")
        |> shouldBe (Success "1337code")

        Result.Map4 
            (Func<int, float, string, string, string> (fun i j k l -> 
                i.ToString() + j.ToString() + k + l))
            (Failure "didn't work")
            (Success 37.)
            (Success "co")
            (Success "de")
        |> shouldBe (Result<string, string>.Failure "didn't work")
        
        Result.Map4 
            (Func<int, float, string, string, string> (fun i j k l -> 
                i.ToString() + j.ToString() + k + l))
            (Success 13)
            (Success 37.)
            (Success "co")
            (Failure "didn't work")
        |> shouldBe (Result<string, string>.Failure "didn't work")
    
    [<Test>]
    let ``can bind four results`` () =
        Result.Bind4 
            (Func<int, float, string, string, Result<string,string>> (fun i j k l -> 
                Success (i.ToString() + j.ToString() + k + l)))
            (Success 13)
            (Success 37.)
            (Success "co")
            (Success "de")
        |> shouldBe (Result<string, string>.Success "1337code")

        Result.Bind4 
            (Func<int, float, string, string, Result<string,string>> (fun i j k l -> 
                Success (i.ToString() + j.ToString() + k + l)))
            (Failure "didn't work")
            (Success 37.)
            (Success "co")
            (Success "de")
        |> shouldBe (Result<string, string>.Failure "didn't work")
        
        Result.Bind4 
            (Func<int, float, string, string, Result<string,string>> (fun i j k l -> 
                Success (i.ToString() + j.ToString() + k + l)))
            (Success 13)
            (Success 37.)
            (Success "co")
            (Failure "didn't work")
        |> shouldBe (Result<string, string>.Failure "didn't work")
    