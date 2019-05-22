namespace ResultDotNet
open System.Runtime.CompilerServices

/// <summary>
/// Static functions for working with Result objects 
/// </summary>
module Result =
  open System
  open FuncTransforms
  
  /// <summary>
  /// If the Result is ok, "unwraps" the ok value and passes it
  /// to the function given, returning the result of that function.  If the 
  /// Result is error, returns the error without calling the function given. 
  /// </summary>
  let Bind onOk (result:Result<_,_>) = result.Bind onOk

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  let Bind2 (onOk:Func<'a, 'b, Result<'c, 'd>>) (result1:Result<'a,'d>) (result2:Result<'b,'d>) = 
    result1.Bind(fun r1 ->
      result2.Bind(fun r2 -> onOk.Invoke (r1, r2)))

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  let Bind3 (onOk:Func<'a, 'b, 'c, Result<'d, 'e>>) (result1:Result<'a,'e>) (result2:Result<'b,'e>) (result3:Result<'c,'e>) = 
    result1.Bind(fun r1 ->
      result2.Bind(fun r2 -> 
        result3.Bind(fun r3 -> onOk.Invoke (r1, r2, r3))))

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  let Bind4 (onOk:Func<'a, 'b, 'c, 'd, Result<'e, 'f>>) (result1:Result<'a,'f>) (result2:Result<'b,'f>) (result3:Result<'c,'f>) (result4:Result<'d,'f>) = 
    result1.Bind(fun r1 ->
      result2.Bind(fun r2 -> 
        result3.Bind(fun r3 -> 
          result4.Bind(fun r4 -> onOk.Invoke (r1, r2, r3, r4)))))

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
  let BindAll onOk results = 
    (concatResults results).Bind onOk

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes it
  /// to the function given, returning an Ok with the result of that function.  
  /// If the Result is error, returns the error without calling the function given. 
  /// </summary>
  let Map onOk (result:Result<_,_>) = result.Map onOk

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  let Map2 (onOk:Func<'a, 'b, 'c>) result1 result2 = 
    Bind2 (Func<'a, 'b, Result<'c, 'd>> (fun r1 r2 -> Ok (onOk.Invoke (r1, r2)))) result1 result2

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  let Map3 (onOk:Func<'a, 'b, 'c, 'd>) result1 result2 result3 = 
    Bind3 (Func<'a, 'b, 'c, Result<'d, 'e>> (fun r1 r2 r3 -> Ok (onOk.Invoke (r1, r2, r3)))) 
      result1 result2 result3

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  let Map4 (onOk:Func<'a, 'b, 'c, 'd, 'e>) result1 result2 result3 result4 = 
    Bind4 (Func<'a, 'b, 'c, 'd, Result<'e, 'f>> (fun r1 r2 r3 r4 -> Ok (onOk.Invoke (r1, r2, r3, r4)))) 
      result1 result2 result3 result4

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  let MapAll onOk results = 
    (concatResults results).Map onOk

  /// <summary>
  /// Creates a new ok Result with the value given. 
  /// </summary>
  let Ok<'tVal, 'tErr> v : Result<'tVal, 'tErr> = Ok v

  /// <summary>
  /// Creates a new error Result with the error object given. 
  /// </summary>
  let Error<'tVal, 'tErr> v : Result<'tVal, 'tErr> = Error v

  /// <summary>
  /// Converts an FSharpResult type to a ResultDotNet Result type.  Used for interoping between
  /// C# and F# codebases. 
  /// </summary>
  let FromFs v = 
    ResultDotNet.FSharp.Result.toCs v

  /// <summary>
  /// Collects a sequence of Results into a single Result of the sequence of values.
  /// If all of the Results are Ok, returns an Ok of the sequence of contained values.  
  /// If any of the Results are Error, returns an Error of the sequence of contained 
  /// Errors (and throws away any Ok values). 
  /// <c>Result.Collect [Ok(1), Ok(2), Ok(3)]</c> would return <c>Ok([1, 2, 3])</c>, but 
  /// <c>Result.Collect [Ok(1), Error("err"), Error("fail")]</c> would return <c>Error(["err", "fail"])</c>
  /// </summary>
  let Collect results = ResultDotNet.FSharp.Result.collect results |> FromFs

[<AutoOpen>]
[<Extension>]
module FsResultExtensions = 
  /// <summary>
  /// Converts to a ResultDotNet Result type.  Used for interoping between C# and F# codebases.
  /// </summary>
  [<Extension>]
  let ToCs v = Result.FromFs v
