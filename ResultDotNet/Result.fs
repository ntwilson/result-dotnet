namespace ResultDotNet

open System

type Result<'tVal, 'tErr> =
  | Success of 'tVal
  | Failure of 'tErr

   
  member this.Match (success:Func<'tVal, 'a>, failure:Func<'tErr, 'a>) =
    match this with
    | Success v -> success.Invoke v
    | Failure err -> failure.Invoke err

  member this.Match (success:Action<'tVal>, failure:Action<'tErr>) =
    this.Match (
      Func<'tVal, unit> success.Invoke,
      Func<'tErr, unit> failure.Invoke)

  member this.Bind (onSuccess:Func<'tVal, Result<'a, 'tErr>>) =
    match this with
    | Success v -> onSuccess.Invoke v
    | Failure err -> Failure err

  member this.Map (onSuccess:Func<'tVal, 'a>) = 
    match this with
    | Success v -> Success (onSuccess.Invoke v)
    | Failure err -> Failure err

