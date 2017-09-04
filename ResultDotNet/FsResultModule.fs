namespace ResultDotNet.FSharp

open ResultDotNet.FuncTransforms

/// <summary>
/// Used for computation expressions.  Use <c>Result.expr</c> to create the expression. 
/// </summary>
type ResultExpression () =
  member this.Bind (x, onOk) = Result.bind onOk x
  member this.Return x = Ok x
  member this.ReturnFrom x = x

/// <summary>
/// Static functions for working with Result objects 
/// </summary>
module Result =
  open System
  
  /// <summary>
  /// Create a computation expression for Results 
  /// </summary>
  let expr = new ResultExpression ()

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  let bind2 onOk result1 result2 =
    expr {
      let! r1 = result1
      let! r2 = result2
      return! onOk r1 r2
    }

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  let bind3 onOk result1 result2 result3 =
    expr {
      let! r1 = result1
      let! r2 = result2
      let! r3 = result3
      return! onOk r1 r2 r3
    }

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
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

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  let bindAll onOk results = 
    concatResults results
    |> Result.bind onOk

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  let map2 onOk result1 result2 = 
    expr {
      let! r1 = result1
      let! r2 = result2
      return onOk r1 r2
    }

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  let map3 onOk result1 result2 result3 = 
    expr {
      let! r1 = result1
      let! r2 = result2
      let! r3 = result3
      return onOk r1 r2 r3
    }

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  let map4 onOk result1 result2 result3 result4 =
    expr {
      let! r1 = result1
      let! r2 = result2
      let! r3 = result3
      let! r4 = result4
      return onOk r1 r2 r3 r4
    }
          
  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  let mapAll onOk results = 
    concatResults results
    |> Result.map onOk

  /// <summary>
  /// If the Result is ok, "unwraps" the ok value and passes it 
  /// to the function passed in.  Does nothing if the Result is an error
  /// </summary>
  let ifOk ok result = 
    match result with
    | Ok v -> ok v
    | Error _ -> () 

  /// <summary>
  /// If the Result is error, "unwraps" the error object and passes it 
  /// to the function passed in.  Does nothing if the Result is an ok
  /// </summary>
  let ifError error result = 
    match result with
    | Ok _ -> ()
    | Error err -> error err

  let defaultValue value result = 
    match result with
    | Ok v -> v
    | Error err -> value

  let defaultWith valueF result =
    match result with
    | Ok v -> v
    | Error err -> valueF err
