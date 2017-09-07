namespace ResultDotNet

open System

/// <summary>
/// Represents the outcome of a calculation that could have failed.
/// For example, a divide function might return a Result, with an Error
/// if the denominator was 0, and an Ok in any other case.
/// Result is a union of Ok<tVal> and Error<tErr>.   
/// tVal represents the type of the expected result, and tErr represents
/// the type used to represent the error (such as a string if just using
/// error messages)
/// </summary>
type Result<'tVal, 'tErr> = 
  | Ok of 'tVal
  | Error of 'tErr

   
  /// <summary>
  /// "Unwraps" the ok value or the error object and passes it 
  /// to the function passed in.
  /// Takes two Funcs: one to execute if the result is ok, and one
  /// to execute if the result is an error.  Returns the result of executing
  /// the appropriate function.
  /// </summary>
  member this.Match (ok:Func<'tVal, 'a>, error:Func<'tErr, 'a>) =
    match this with
    | Ok v -> ok.Invoke v
    | Error err -> error.Invoke err

  /// <summary>
  /// "Unwraps" the ok value or the error object and passes it 
  /// to the function passed in.
  /// Takes two Actions: one to execute if the result is ok, and one 
  /// to execute if the result is an error.
  /// </summary>
  member this.Match (ok:Action<'tVal>, error:Action<'tErr>) =
    this.Match (
      Func<'tVal, unit> ok.Invoke,
      Func<'tErr, unit> error.Invoke)

  /// <summary>
  /// If the Result is ok, "unwraps" the ok value and passes it 
  /// to the function passed in.  Does nothing if the Result is an error
  /// </summary>
  member this.IfOk (ok:Action<'tVal>) =
    this.Match (ok, ignore)

  /// <summary>
  /// If the Result is error, "unwraps" the error object and passes it 
  /// to the function passed in.  Does nothing if the Result is an ok
  /// </summary>
  member this.IfError (error:Action<'tErr>) =
    this.Match (ignore, error)

  /// <summary>
  /// If the result is error, returns the defaultValue passed in.  If 
  /// the result is ok, "unwraps" the ok value and returns it.
  /// </summary>
  member this.OkOrElse defaultValue =
    match this with
    | Ok v -> v
    | Error _ -> defaultValue

  /// <summary>
  /// If the result is error, returns the result of the defaultValueFunc 
  /// passed in.  If the result is ok, "unwraps" the ok value
  /// and returns it.
  /// </summary>
  member this.OkOrElse (defaultValueFunc:Func<'tVal>) =
    match this with
    | Ok v -> v
    | Error _ -> defaultValueFunc.Invoke ()

  /// <summary>
  /// If the result is error, returns the result of the defaultValueFunc 
  /// passed in.  If the result is ok, "unwraps" the ok value
  /// and returns it.
  /// </summary>
  member this.OkOrElse (defaultValueFunc:Func<'tErr, 'tVal>) =
    match this with
    | Ok v -> v
    | Error err -> defaultValueFunc.Invoke err

  /// <summary>
  /// If the Result is ok, "unwraps" the ok value and passes it
  /// to the function given, returning the result of that function.  If the 
  /// Result is error, returns the error without calling the function given. 
  /// </summary>
  member this.Bind (onOk:Func<'tVal, Result<'a, 'tErr>>) =
    match this with
    | Ok v -> onOk.Invoke v
    | Error err -> Error err

  /// <summary>
  /// If the Result is ok, "unwraps" the ok value and passes it
  /// to the function given, returning an Ok with the result of that function.  
  /// If the Result is error, returns the error without calling the function given. 
  /// </summary>
  member this.Map (onOk:Func<'tVal, 'a>) = 
    match this with
    | Ok v -> Ok (onOk.Invoke v)
    | Error err -> Error err

  member this.ToFs () = 
    match this with
    | Ok v -> FSharp.Core.Ok v
    | Error err -> FSharp.Core.Error err

  member this.SelectMany<'uVal, 'vVal> (func : Func<'tVal, Result<'uVal, 'tErr>>) (projection : Func<'tVal, 'uVal, 'vVal>) =
    this.Bind (fun t -> (func.Invoke t).Map (fun u -> projection.Invoke (t, u)))

  member this.Select<'u>(func : Func<'tVal, 'u>) =
    this.Map func