namespace ResultDotNet.Specs

open Xunit

module ResultSpec =
  open Should
  open ResultDotNet
  open ResultDotNet.FSharp
  open System

  [<Fact>]
  let ``should have structural equality`` () =
    Success 5 |> shouldBe (Success 5)
    Failure "didn't work" |> shouldBe (Failure "didn't work")

    Success 5 |> shouldNotBe (Success 10)
    Failure "didn't work" |> shouldNotBe (Failure "hello world")

  [<Fact>]
  let ``static functions behave the same as the members`` () =
    Success 5 |> Result.map ((+) 3)
    |> shouldBe (Success 8)

    Failure "didn't work" |> Result.map ((+) 3)
    |> shouldBe (Result.Failure<int, string> "didn't work")
    
    Success 5 |> Result.bind (fun i -> Success (i + 3))
    |> shouldBe (Success 8)

    Failure "didn't work" |> Result.bind (fun i -> Success (i + 3))
    |> shouldBe (Result.Failure<int, string> "didn't work")

  [<Fact>]
  let ``can map two results`` () =
    Result.map2 (fun i j -> (float i) + j)
      (Success 3)
      (Success 5.)
    |> shouldBe (Success 8.)

    Result.map2 (fun i j -> (float i) + j)
      (Failure "didn't work")
      (Success 5.)
    |> shouldBe (Result.Failure<float, string> "didn't work")
        
    Result.map2 (fun i j -> (float i) + j)
      (Success 3)
      (Failure "didn't work")
    |> shouldBe (Result.Failure<float, string> "didn't work")

  [<Fact>]
  let ``can bind two results`` () =
    Result.bind2 (fun i j -> Success ((float i) + j))
      (Success 3)
      (Success 5.)
    |> shouldBe (Success 8.)

    Result.bind2 (fun i j -> Success ((float i) + j))
      (Failure "didn't work")
      (Success 5.)
    |> shouldBe (Result.Failure<float, string> "didn't work")
        
    Result.bind2 (fun i j -> Success ((float i) + j))
      (Success 3)
      (Failure "didn't work")
    |> shouldBe (Result.Failure<float, string> "didn't work")

  [<Fact>]
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
    |> shouldBe (Result.Failure<string, string> "didn't work")
        
    Result.map3 (fun i j k -> i.ToString() + j.ToString() + k)
      (Success 13)
      (Success 37.)
      (Failure "didn't work")
    |> shouldBe (Result.Failure<string, string> "didn't work")
    
  [<Fact>]
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
    |> shouldBe (Result.Failure<string, string> "didn't work")
        
    Result.bind3 (fun i j k -> Success (i.ToString() + j.ToString() + k))
      (Success 13)
      (Success 37.)
      (Failure "didn't work")
    |> shouldBe (Result.Failure<string, string> "didn't work")
    
  [<Fact>]
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
    |> shouldBe (Result.Failure<string, string> "didn't work")
        
    Result.map4 (fun i j k l -> i.ToString() + j.ToString() + k + l)
      (Success 13)
      (Success 37.)
      (Success "co")
      (Failure "didn't work")
    |> shouldBe (Result.Failure<string, string> "didn't work")
    
  [<Fact>]
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
    |> shouldBe (Result.Failure<string, string> "didn't work")
        
    Result.bind4 (fun i j k l -> Success (i.ToString() + j.ToString() + k + l))
      (Success 13)
      (Success 37.)
      (Success "co")
      (Failure "didn't work")
    |> shouldBe (Result.Failure<string, string> "didn't work")
    
  [<Fact>]
  let ``can bind N results as an array`` () =
    Result.bindAll ((String.concat " ") >> Success)
      ([|"Results";"are";"better";"than";"exceptions"|] |> Seq.map Success)
    |> shouldBe (Success "Results are better than exceptions")

    Result.bindAll (fun args -> Success (Seq.reduce (+) args))
      ([|Success 0; Success 1; Success 2; Failure "didn't work"; Success 4|])
    |> shouldBe (Result.Failure<int, string> "didn't work")
        
  [<Fact>]
  let ``can map N results as an array`` () =
    Result.mapAll (fun args -> String.concat " " args)
      ([|"Results";"are";"better";"than";"exceptions"|] |> Seq.map Success)
    |> shouldBe (Success "Results are better than exceptions")

    Result.mapAll (fun args -> Seq.reduce (+) args)
      ([|Success 0; Success 1; Success 2; Failure "didn't work"; Success 4|])
    |> shouldBe (Result.Failure<int, string> "didn't work")

  [<Fact>]
  let ``can execute an action on just success or on just failure`` () =
    let mutable didRun = false
    (Success 5) |> Result.ifSuccess (fun i -> (didRun <- true))
    didRun |> shouldBe true

    didRun <- false
    (Failure "didn't work")|> Result.ifSuccess (fun i -> (didRun <- true))
    didRun|> shouldBe false

    didRun <- false
    (Failure "didn't work")|> Result.ifFailure (fun err -> (didRun <- true))
    didRun|> shouldBe true

    didRun <- false
    (Success 5)|> Result.ifFailure (fun err -> (didRun <- true))
    didRun|> shouldBe false

  [<Fact>]
  let ``can use computation expressions to bind and map Results`` () =
    let tryToGetA = Success 5
    let tryToGetB = Success 10
    let tryToGetC = Failure "didn't work"
    let add x y = x + y
    let add' x y = Success (x + y)

    Result.expr {
      let! x = tryToGetA
      let! y = tryToGetB
      return add x y }
    |> shouldBe (Success 15)

    Result.expr {
      let! x = tryToGetA
      let! y = tryToGetC
      return add x y }
    |> shouldBe (Result.Failure<int, string> "didn't work")

    Result.expr {
      let! x = tryToGetA
      let y = 10
      return! add' x y }
    |> shouldBe (Success 15)