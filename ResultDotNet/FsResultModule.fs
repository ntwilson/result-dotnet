namespace ResultDotNet.FSharp

open ResultDotNet.FuncTransforms

/// <summary>
/// Used for computation expressions.  Use <c>Result.expr</c> or <c>ResultExpression.result</c> 
/// to create the expression. 
/// </summary>
type ResultExpression () =
  member this.Bind (x, onOk) = Result.bind onOk x
  member this.Return x = Ok x
  member this.ReturnFrom x = x

[<AutoOpen>]
module ResultExpression =
  /// <summary>
  /// Create a computation expression for Results
  /// </summary>
  let result = new ResultExpression ()

/// <summary>
/// Static functions for working with Result objects 
/// </summary>
module Result =
  open System

  let createContext errContext result = 
    result
    |> Result.mapError (fun e -> 
      { ResultDotNet.ErrorWithContext.Error = e; ResultDotNet.ErrorWithContext.Context = [errContext] })

  let addContext newContext result = 
    result
    |> Result.mapError (fun ({ ResultDotNet.ErrorWithContext.Context = context } as error) -> 
      { error with Context = newContext :: context })

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

  /// <summary>
  /// Collects a sequence of Results into a single Result of the sequence of values.
  /// If all of the Results are Ok, returns an Ok of the sequence of contained values.  
  /// If any of the Results are Error, returns an Error of the sequence of contained 
  /// Errors (and throws away any Ok values). 
  /// <c>collect [Ok 1; Ok 2; Ok 3]</c> would return <c>Ok [1; 2; 3]</c>, but 
  /// <c>collect [Ok 1; Error "err"; Error "fail"]</c> would return <c>Error ["err"; "fail"]</c>
  /// </summary>
  let collect results =
    Seq.fold 
      (fun state element -> 
        match state, element with
        | (Ok xs, Ok x) -> Ok (Seq.append xs [x])
        | (Ok _, Error x) -> Error (seq [x]) 
        | (Error xs, Error x) -> Error (Seq.append xs [x])
        | (Error xs, Ok _) -> Error xs)
      (Ok (seq []))
      results

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
  /// Returns true when given a Result that is Ok, and false when given a Result that is Error.
  /// </summary>
  let isOk = function | Ok _ -> true | Error _ -> false

  /// <summary>
  /// Returns true when given a Result that is Error, and false when given a Result that is Ok.
  /// </summary>
  let isError = function | Error _ -> true | Ok _ -> false 

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
  /// to the function passed in.  Does nothing if the Result is an ok.
  /// </summary>
  let ifError error result = 
    match result with
    | Ok _ -> ()
    | Error err -> error err

  /// <summary>
  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, returns the default value given.
  /// </summary>
  let defaultValue value result = 
    match result with
    | Ok v -> v
    | Error err -> value

  /// <summary>
  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, constructs the default value from the function given
  /// and returns it.
  /// </summary>
  let defaultWith valueF result =
    match result with
    | Ok v -> v
    | Error err -> valueF err

  /// <summary>
  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, throws a ResultExpectedException containing the 
  /// specified message along with the error details
  /// </summary>
  let unless msg result = 
    match result with
    | Ok v -> v
    | Error err -> raise (ResultDotNet.ResultExpectedException (msg, err))

  /// <summary>
  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, throws a ResultExpectedException with a message 
  /// produced by the input function along with the error details
  /// </summary>
  let unlessErr msg result =
    match result with
    | Ok v -> v
    | Error err -> raise (ResultDotNet.ResultExpectedException (msg err, err))

  /// <summary>
  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, throws a ResultExpectedException containing the 
  /// error details
  /// </summary>
  let expect result = 
    match result with
    | Ok v -> v
    | Error err -> raise (ResultDotNet.ResultExpectedException err)

  /// <summary>
  /// Converts a ResultDotNet Result type to an FSharpResult type.  Used for interoping between
  /// C# and F# codebases. 
  /// </summary>
  let ofCs (result:ResultDotNet.Result<_,_>) = 
    result.ToFs()

  /// <summary>
  /// Converts an FSharpResult type to a ResultDotNet Result type.  Used for interoping between
  /// C# and F# codebases. 
  /// </summary>
  let toCs result = 
    match result with
    | Ok ok -> ResultDotNet.Result.Ok ok
    | Error err -> ResultDotNet.Result.Error err

  /// <summary>
  /// Converts an <c>Option</c> of <c>'a</c> to a <c>Result</c> of <c>'a</c> and <c>'err</c>
  /// given some <c>'err</c> to use if the <c>Option</c> is <c>None</c> 
  /// </summary>
  let ofOption err opt = 
    match opt with
    | Some v -> Ok v
    | None -> Error err

  /// <summary>
  /// Converts an <c>Option</c> of <c>'a</c> to a <c>Result</c> of <c>'a</c> and <c>'err</c>
  /// given a delayed function of <c>'err</c> to use if the <c>Option</c> is <c>None</c> 
  /// </summary>
  let ofOptionWith errThunk opt = 
    match opt with
    | Some v -> Ok v
    | None -> Error (errThunk ())

  /// <summary>
  /// Converts a <c>Result</c> to an <c>Option</c>, throwing away the <c>Error</c> information
  /// if the <c>Result</c> was an <c>Error</c>
  /// </summary>
  let toOption result = 
    match result with
    | Ok v -> Some v
    | Error _ -> None  
