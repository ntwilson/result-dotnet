namespace ResultDotNet.FSharp

open ResultDotNet.FuncTransforms

/// Used for computation expressions.  Use <c>Result.expr</c> or <c>ResultExpression.result</c> 
/// to create the expression. 
type ResultExpression () =
  member this.Bind (x, onOk) = Result.bind onOk x
  member this.Return x = Ok x
  member this.ReturnFrom x = x

[<AutoOpen>]
module ResultExpression =
  /// Create a computation expression for Results
  let result = new ResultExpression ()

/// Static functions for working with Result objects 
module Result =
  open System
  
  /// Create a computation expression for Results 
  let expr = new ResultExpression ()

  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  let bind2 onOk result1 result2 =
    expr {
      let! r1 = result1
      let! r2 = result2
      return! onOk r1 r2
    }

  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  let bind3 onOk result1 result2 result3 =
    expr {
      let! r1 = result1
      let! r2 = result2
      let! r3 = result3
      return! onOk r1 r2 r3
    }

  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  let bind4 onOk result1 result2 result3 result4 =
    expr {
      let! r1 = result1
      let! r2 = result2
      let! r3 = result3
      let! r4 = result4
      return! onOk r1 r2 r3 r4
    }

  let private concatResults results =
    let rec concat state rs =
      match rs with
      | head::tail -> 
        match head with 
        | Ok x -> concat (x::state) tail
        | Error err -> Error err
      | [] -> Ok (state |> Seq.ofList |> Seq.rev)

    concat [] (results |> Seq.toList)

  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  let bindAll onOk results = 
    concatResults results
    |> Result.bind onOk

  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  let map2 onOk result1 result2 = 
    expr {
      let! r1 = result1
      let! r2 = result2
      return onOk r1 r2
    }

  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  let map3 onOk result1 result2 result3 = 
    expr {
      let! r1 = result1
      let! r2 = result2
      let! r3 = result3
      return onOk r1 r2 r3
    }

  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  let map4 onOk result1 result2 result3 result4 =
    expr {
      let! r1 = result1
      let! r2 = result2
      let! r3 = result3
      let! r4 = result4
      return onOk r1 r2 r3 r4
    }
          
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  let mapAll onOk results = 
    concatResults results
    |> Result.map onOk

  /// If the Result is ok, "unwraps" the ok value and passes it 
  /// to the function passed in.  Does nothing if the Result is an error
  let ifOk ok result = 
    match result with
    | Ok v -> ok v
    | Error _ -> () 

  /// If the Result is error, "unwraps" the error object and passes it 
  /// to the function passed in.  Does nothing if the Result is an ok.
  let ifError error result = 
    match result with
    | Ok _ -> ()
    | Error err -> error err

  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, returns the default value given.
  let defaultValue value result = 
    match result with
    | Ok v -> v
    | Error err -> value

  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, constructs the default value from the function given
  /// and returns it.
  let defaultWith valueF result =
    match result with
    | Ok v -> v
    | Error err -> valueF err

  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, throws a ResultExpectedException containing the 
  /// specified message along with the error details
  let unless msg result = 
    match result with
    | Ok v -> v
    | Error err -> raise (ResultDotNet.ResultExpectedException (msg, err))

  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, throws a ResultExpectedException containing the 
  /// error details
  let expect result = 
    match result with
    | Ok v -> v
    | Error err -> raise (ResultDotNet.ResultExpectedException err)