module ResultDotNet.Specs.ResultSpec 
open System
open NUnit.Framework

open Should
open ResultDotNet.FSharp

[<Test>]
let ``should have structural equality`` () =
  Ok 5 |> shouldBe (Ok 5)
  Error "didn't work" |> shouldBe (Error "didn't work")

  Ok 5 |> shouldNotBe (Ok 10)
  Error "didn't work" |> shouldNotBe (Error "hello world")

[<Test>]
let ``static functions behave the same as the members`` () =
  Ok 5 |> Result.map ((+) 3)
  |> shouldBe (Ok 8)

  Error "didn't work" |> Result.map ((+) 3)
  |> shouldBe (Error "didn't work")
  
  Ok 5 |> Result.bind (fun i -> Ok (i + 3))
  |> shouldBe (Ok 8)

  Error "didn't work" |> Result.bind (fun i -> Ok (i + 3))
  |> shouldBe (Error "didn't work")

[<Test>]
let ``can map two results`` () =
  Result.map2 (fun i j -> (float i) + j)
    (Ok 3)
    (Ok 5.)
  |> shouldBe (Ok 8.)

  Result.map2 (fun i j -> (float i) + j)
    (Error "didn't work")
    (Ok 5.)
  |> shouldBe (Error "didn't work")
      
  Result.map2 (fun i j -> (float i) + j)
    (Ok 3)
    (Error "didn't work")
  |> shouldBe (Error "didn't work")

[<Test>]
let ``can bind two results`` () =
  Result.bind2 (fun i j -> Ok ((float i) + j))
    (Ok 3)
    (Ok 5.)
  |> shouldBe (Ok 8.)

  Result.bind2 (fun i j -> Ok ((float i) + j))
    (Error "didn't work")
    (Ok 5.)
  |> shouldBe (Error "didn't work")
      
  Result.bind2 (fun i j -> Ok ((float i) + j))
    (Ok 3)
    (Error "didn't work")
  |> shouldBe (Error "didn't work")

[<Test>]
let ``can map three results`` () =
  Result.map3 (fun i j k -> i.ToString() + j.ToString() + k)
    (Ok 13)
    (Ok 37.)
    (Ok "code")
  |> shouldBe (Ok "1337code")

  Result.map3 (fun i j k -> i.ToString() + j.ToString() + k)
    (Error "didn't work")
    (Ok 37.)
    (Ok "code")
  |> shouldBe (Error "didn't work")
      
  Result.map3 (fun i j k -> i.ToString() + j.ToString() + k)
    (Ok 13)
    (Ok 37.)
    (Error "didn't work")
  |> shouldBe (Error "didn't work")
  
[<Test>]
let ``can bind three results`` () =
  Result.bind3 (fun i j k -> Ok (i.ToString() + j.ToString() + k))
    (Ok 13)
    (Ok 37.)
    (Ok "code")
  |> shouldBe (Ok "1337code")

  Result.bind3 (fun i j k -> Ok (i.ToString() + j.ToString() + k))
    (Error "didn't work")
    (Ok 37.)
    (Ok "code")
  |> shouldBe (Error "didn't work")
      
  Result.bind3 (fun i j k -> Ok (i.ToString() + j.ToString() + k))
    (Ok 13)
    (Ok 37.)
    (Error "didn't work")
  |> shouldBe (Error "didn't work")
  
[<Test>]
let ``can map four results`` () =
  Result.map4 (fun i j k l -> i.ToString() + j.ToString() + k + l)
    (Ok 13)
    (Ok 37.)
    (Ok "co")
    (Ok "de")
  |> shouldBe (Ok "1337code")

  Result.map4 (fun i j k l -> i.ToString() + j.ToString() + k + l)
    (Error "didn't work")
    (Ok 37.)
    (Ok "co")
    (Ok "de")
  |> shouldBe (Error "didn't work")
      
  Result.map4 (fun i j k l -> i.ToString() + j.ToString() + k + l)
    (Ok 13)
    (Ok 37.)
    (Ok "co")
    (Error "didn't work")
  |> shouldBe (Error "didn't work")
  
[<Test>]
let ``can bind four results`` () =
  Result.bind4 (fun i j k l -> Ok (i.ToString() + j.ToString() + k + l))
    (Ok 13)
    (Ok 37.)
    (Ok "co")
    (Ok "de")
  |> shouldBe (Ok "1337code")

  Result.bind4 (fun i j k l -> Ok (i.ToString() + j.ToString() + k + l))
    (Error "didn't work")
    (Ok 37.)
    (Ok "co")
    (Ok "de")
  |> shouldBe (Error "didn't work")
      
  Result.bind4 (fun i j k l -> Ok (i.ToString() + j.ToString() + k + l))
    (Ok 13)
    (Ok 37.)
    (Ok "co")
    (Error "didn't work")
  |> shouldBe (Error "didn't work")
  
[<Test>]
let ``can bind N results as an array`` () =
  Result.bindAll ((String.concat " ") >> Ok)
    ([|"Results";"are";"better";"than";"exceptions"|] |> Seq.map Ok)
  |> shouldBe (Ok "Results are better than exceptions")

  Result.bindAll (fun args -> Ok (Seq.reduce (+) args))
    ([|Ok 0; Ok 1; Ok 2; Error "didn't work"; Ok 4|])
  |> shouldBe (Error "didn't work")
      
[<Test>]
let ``can map N results as an array`` () =
  Result.mapAll (fun args -> String.concat " " args)
    ([|"Results";"are";"better";"than";"exceptions"|] |> Seq.map Ok)
  |> shouldBe (Ok "Results are better than exceptions")

  Result.mapAll (fun args -> Seq.reduce (+) args)
    ([|Ok 0; Ok 1; Ok 2; Error "didn't work"; Ok 4|])
  |> shouldBe (Error "didn't work")

[<Test>]
let ``can execute an action on just ok or on just error`` () =
  let mutable didRun = false
  (Ok 5) |> Result.ifOk (fun i -> (didRun <- true))
  didRun |> shouldBe true

  didRun <- false
  (Error "didn't work")|> Result.ifOk (fun i -> (didRun <- true))
  didRun|> shouldBe false

  didRun <- false
  (Error "didn't work")|> Result.ifError (fun err -> (didRun <- true))
  didRun|> shouldBe true

  didRun <- false
  (Ok 5)|> Result.ifError (fun err -> (didRun <- true))
  didRun|> shouldBe false

[<Test>]
let ``can use computation expressions to bind and map Results`` () =
  let tryToGetA = Ok 5
  let tryToGetB = Ok 10
  let tryToGetC = Error "didn't work"
  let add x y = x + y
  let add' x y = Ok (x + y)

  result {
    let! x = tryToGetA
    let! y = tryToGetB
    return add x y }
  |> shouldBe (Ok 15)

  result {
    let! x = tryToGetA
    let! y = tryToGetC
    return add x y }
  |> shouldBe (Error "didn't work")

  result {
    let! x = tryToGetA
    let y = 10
    return! add' x y }
  |> shouldBe (Ok 15)


[<Test>]
let ``can use a default value for a failed result`` () =
  Error "didn't work" |> Result.defaultValue 10 |> shouldBe 10
  Ok "yay" |> Result.defaultValue "it didn't work" |> shouldBe "yay"

  Error "didn't work" |> Result.defaultWith (fun _ -> 10) |> shouldBe 10
  Ok "yay" |> Result.defaultWith (fun _ -> "it didn't work") |> shouldBe "yay"


let expectException act =
  try 
    let (Lazy result) = act 
    raise (AssertionException "Expected an Exception to be thrown, but none was") 
  with  
  | ex -> ex

  
[<Test>]
let ```unless` will extract the value from a Result with a message`` () = 
  Ok "yay" |> Result.unless "Expected to be able to extract a value from an Ok Result"
  |> shouldBe "yay"

  let ex = 
    expectException (
      lazy (Error "Didn't work" |> Result.unless "Can't find"))

  ex.Message |> shouldSatisfy (fun s -> s.Contains "Didn't work" && s.Contains "Can't find")
  match ex with
  | :? ResultDotNet.ResultExpectedException<string> as rex -> 
    rex.ErrorDetails |> shouldBe "Didn't work"
  | _ -> 
    raise (AssertionException (sprintf "Expected a ResultExpectedException, but got a %s" (ex.GetType().FullName)))

[<Test>]
let ```expect` will extract the value from a Result without a message`` () = 
  Ok "yay" |> Result.expect
  |> shouldBe "yay"

  let ex = 
    expectException (
      lazy (Error "Didn't work" |> Result.expect))

  ex.Message |> shouldSatisfy (fun s -> s.Contains "Didn't work")
  match ex with
  | :? ResultDotNet.ResultExpectedException<string> as rex -> 
    rex.ErrorDetails |> shouldBe "Didn't work"
  | _ -> 
    raise (AssertionException (sprintf "Expected a ResultExpectedException, but got a %s" (ex.GetType().FullName)))

type SampleErr = Catchable of string
type NotCatchableErr = NotCatchable of string

[<Test>]
let ``expecting a result still lets you catch specific exceptions`` () =
  try 
      Error (Catchable "didn't work") |> Result.expect
      false  
    with  
    | :? ResultDotNet.ResultExpectedException<SampleErr> as ex -> true
  |> shouldBe true

  expectException
    (lazy 
      ( try Error (NotCatchable "didn't work") |> Result.expect with
        | :? ResultDotNet.ResultExpectedException<SampleErr> as ex -> false))
  |> ignore

[<Test>]
let ``can convert to and from an option`` () = 
  Some 4 |> Result.ofOption "didn't work" |> shouldBe (Ok 4)
  Some 4 |> Result.ofOptionWith (fun() -> failwith "shouldn't have run this thunk") 
  |> shouldBe (Ok 4)

  None |> Result.ofOption "didn't work" |> shouldBe (Error "didn't work")
  None |> Result.ofOptionWith (fun() -> "didn't work") |> shouldBe (Error "didn't work")

  Ok 4 |> Result.toOption |> shouldBe (Some 4)
  Error "didn't work" |> Result.toOption |> shouldBe None

[<Test>]
let ``Can tell if a Result is Ok or Error`` () = 
  Error 5 |> Result.isError |> shouldBe true
  Error 5 |> Result.isOk |> shouldBe false

  Ok 5 |> Result.isOk |> shouldBe true
  Ok 5 |> Result.isError |> shouldBe false

[<Test>]
let ``collects sequences of Results into a Result of a sequence of values`` () =
  Result.collect [Ok 1; Ok 2; Ok 3] |> Result.map Seq.toList 
  |> shouldBe (Ok [1; 2; 3])
  
  Result.collect [Ok 1; Error "didn't"; Error "work"] |> Result.mapError Seq.toList 
  |> shouldBe (Error ["didn't"; "work"])


