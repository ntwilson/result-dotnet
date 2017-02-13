namespace ResultDotNet.FSharp

open ResultDotNet
open FuncTransforms

/// <summary>
/// Used for computation expressions.  Use <c>Result.expr</c> to create the expression. 
/// </summary>
type ResultExpression () =
  member this.Bind (x:Result<_,_>, onSuccess) = x.Bind (toCSharpFunc onSuccess)
  member this.Return x = Success x
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
  /// If the Result is success, "unwraps" the successful value and passes it
  /// to the function given, returning the result of that function.  If the 
  /// Result is failure, returns the failure without calling the function given. 
  /// </summary>
  let bind (onSuccess : 'tVal -> Result<'a, 'tErr>) (result:Result<'tVal, 'tErr>) = 
    result.Bind (toCSharpFunc onSuccess)

  /// <summary>
  /// If all the Results are success, "unwraps" the successful values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is failure, returns the first failure without calling the function given. 
  /// </summary>
  let bind2 onSuccess result1 result2 =
    result1 |> bind (fun r1 -> 
      result2 |> bind (fun r2 -> 
        onSuccess r1 r2))

  /// <summary>
  /// If all the Results are success, "unwraps" the successful values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is failure, returns the first failure without calling the function given. 
  /// </summary>
  let bind3 onSuccess result1 result2 result3 =
    result1 |> bind (fun r1 ->
      result2 |> bind (fun r2 -> 
        result3 |> bind (fun r3 ->
          onSuccess r1 r2 r3)))

  /// <summary>
  /// If all the Results are success, "unwraps" the successful values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is failure, returns the first failure without calling the function given. 
  /// </summary>
  let bind4 onSuccess result1 result2 result3 result4 =
    result1 |> bind (fun r1 ->
      result2 |> bind (fun r2 ->
        result3 |> bind (fun r3 ->
          result4 |> bind (fun r4 ->
            onSuccess r1 r2 r3 r4))))

  let private concatResults results =
    let rec concat state rs =
      match rs with
      | head::tail -> 
        match head with 
        | Success x -> concat (x::state) tail
        | Failure err -> Failure err
      | [] -> Success (state |> Seq.ofList |> Seq.rev)

    concat [] (results |> Seq.toList)

  /// <summary>
  /// If all the Results are success, "unwraps" the successful values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is failure, returns the first failure without calling the function given. 
  /// </summary>
  let bindAll onSuccess results = 
    concatResults results
    |> bind onSuccess

  /// <summary>
  /// If all the Results are success, "unwraps" the successful values and passes it
  /// to the function given, returning a Success with the result of that function.  
  /// If the Result is failure, returns the failure without calling the function given. 
  /// </summary>
  let map (onSuccess : 'tVal -> 'a) (result:Result<'tVal, 'tErr>) = 
    result.Map (toCSharpFunc onSuccess)

  /// <summary>
  /// If the Result is success, "unwraps" the successful values and passes them
  /// to the function given, returning a Success with the result of that function.  
  /// If any Result is failure, returns the first failure without calling the function given. 
  /// </summary>
  let map2 onSuccess result1 result2 = 
    result1 |> bind (fun r1 ->
      result2 |> map (fun r2 ->
        onSuccess r1 r2))

  /// <summary>
  /// If the Result is success, "unwraps" the successful values and passes them
  /// to the function given, returning a Success with the result of that function.  
  /// If any Result is failure, returns the first failure without calling the function given. 
  /// </summary>
  let map3 onSuccess result1 result2 result3 = 
    result1 |> bind (fun r1 ->
      result2 |> bind (fun r2 ->
        result3 |> map (fun r3 ->
          onSuccess r1 r2 r3)))

  /// <summary>
  /// If the Result is success, "unwraps" the successful values and passes them
  /// to the function given, returning a Success with the result of that function.  
  /// If any Result is failure, returns the first failure without calling the function given. 
  /// </summary>
  let map4 onSuccess result1 result2 result3 result4 =
    result1 |> bind (fun r1 ->
      result2 |> bind (fun r2 ->
        result3 |> bind (fun r3 ->
          result4 |> map (fun r4 ->
            onSuccess r1 r2 r3 r4))))
          
  /// <summary>
  /// If the Result is success, "unwraps" the successful values and passes them
  /// to the function given, returning a Success with the result of that function.  
  /// If any Result is failure, returns the first failure without calling the function given. 
  /// </summary>
  let mapAll onSuccess results = 
    concatResults results
    |> map onSuccess

  /// <summary>
  /// If the Result is successful, "unwraps" the successful value and passes it 
  /// to the function passed in.  Does nothing if the Result is a failure
  /// </summary>
  let ifSuccess success (result:Result<_,_>) = result.IfSuccess (Action<_> success)

  /// <summary>
  /// If the Result is failure, "unwraps" the error object and passes it 
  /// to the function passed in.  Does nothing if the Result is a success
  /// </summary>
  let ifFailure failure (result:Result<_,_>) = result.IfFailure (Action<_> failure)