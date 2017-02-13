namespace ResultDotNet

open System

/// <summary>
/// Represents the outcome of a calculation that could have failed.
/// For example, a divide function might return a Result, with a Failure
/// if the denominator was 0, and a Success in any other case.
/// Result is a union of Success<tVal> and Failure<tErr>.   
/// tVal represents the type of the expected result, and tErr represents
/// the type used to represent the error (such as a string if just using
/// error messages)
/// </summary>
type Result<'tVal, 'tErr> =
  | Success of 'tVal
  | Failure of 'tErr

   
  /// <summary>
  /// "Unwraps" the successful value or the error object and passes it 
  /// to the function passed in.
  /// Takes two Funcs: one to execute if the result is successful, and one
  /// to execute if the result is a failure.  Returns the result of executing
  /// the appropriate function.
  /// </summary>
  member this.Match (success:Func<'tVal, 'a>, failure:Func<'tErr, 'a>) =
    match this with
    | Success v -> success.Invoke v
    | Failure err -> failure.Invoke err

  /// <summary>
  /// "Unwraps" the successful value or the error object and passes it 
  /// to the function passed in.
  /// Takes two Actions: one to execute if the result is successful, and one 
  /// to execute if the result is a failure.
  /// </summary>
  member this.Match (success:Action<'tVal>, failure:Action<'tErr>) =
    this.Match (
      Func<'tVal, unit> success.Invoke,
      Func<'tErr, unit> failure.Invoke)

  /// <summary>
  /// If the Result is successful, "unwraps" the successful value and passes it 
  /// to the function passed in.  Does nothing if the Result is a failure
  /// </summary>
  member this.IfSuccess (success:Action<'tVal>) =
    this.Match (success, ignore)

  /// <summary>
  /// If the Result is failure, "unwraps" the error object and passes it 
  /// to the function passed in.  Does nothing if the Result is a success
  /// </summary>
  member this.IfFailure (failure:Action<'tErr>) =
    this.Match (ignore, failure)

  /// <summary>
  /// If the Result is success, "unwraps" the successful value and passes it
  /// to the function given, returning the result of that function.  If the 
  /// Result is failure, returns the failure without calling the function given. 
  /// </summary>
  member this.Bind (onSuccess:Func<'tVal, Result<'a, 'tErr>>) =
    match this with
    | Success v -> onSuccess.Invoke v
    | Failure err -> Failure err

  /// <summary>
  /// If the Result is success, "unwraps" the successful value and passes it
  /// to the function given, returning a Success with the result of that function.  
  /// If the Result is failure, returns the failure without calling the function given. 
  /// </summary>
  member this.Map (onSuccess:Func<'tVal, 'a>) = 
    match this with
    | Success v -> Success (onSuccess.Invoke v)
    | Failure err -> Failure err

