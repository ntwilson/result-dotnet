namespace ResultDotNet

open System

type Result<'tVal, 'tErr> =
  | Success of 'tVal
  | Failure of 'tErr
   
  member this.Match success failure =
    match this with
    | Success v -> success v
    | Failure err -> failure err

  member this.Bind (onSuccess:Func<'tVal, Result<'a, 'tErr>>) =
    match this with
    | Success v -> onSuccess.Invoke v
    | Failure err -> Failure err

  member this.Map (onSuccess:Func<'tVal, 'a>) =
    match this with
    | Success v -> Success (onSuccess.Invoke v)
    | Failure err -> Failure err

module Result =
  let toCSharpFunc f = Func<'a, 'b> f

  let Bind onSuccess (result:Result<'tVal, 'tErr>) = result.Bind onSuccess

  let Bind2 (onSuccess:Func<'a, 'b, Result<'c, 'd>>) result1 result2 =
    result1 |> Bind (toCSharpFunc (fun r1 -> 
      result2 |> Bind (toCSharpFunc (fun r2 -> 
        onSuccess.Invoke(r1, r2)))))

  let Bind3 (onSuccess:Func<'a, 'b, 'c, Result<'d, 'e>>) result1 result2 result3 =
    result1 |> Bind (toCSharpFunc (fun r1 ->
      result2 |> Bind (toCSharpFunc (fun r2 -> 
        result3 |> Bind (toCSharpFunc (fun r3 ->
          onSuccess.Invoke(r1, r2, r3)))))))

  let Bind4 (onSuccess:Func<'a, 'b, 'c, 'd, Result<'e, 'f>>) result1 result2 result3 result4 =
    result1 |> Bind (toCSharpFunc (fun r1 ->
      result2 |> Bind (toCSharpFunc (fun r2 ->
        result3 |> Bind (toCSharpFunc (fun r3 ->
          result4 |> Bind (toCSharpFunc (fun r4 ->
            onSuccess.Invoke(r1, r2, r3, r4)))))))))

  let Map onSuccess (result:Result<'tVal, 'tErr>) = result.Map onSuccess

  let Map2 (onSuccess:Func<'a, 'b, 'c>) result1 result2 = 
    result1 |> Bind (toCSharpFunc (fun r1 ->
      result2 |> Map (toCSharpFunc (fun r2 ->
        onSuccess.Invoke(r1, r2)))))

  let Map3 (onSuccess:Func<'a, 'b, 'c, 'd>) result1 result2 result3 = 
    result1 |> Bind (toCSharpFunc (fun r1 ->
      result2 |> Bind (toCSharpFunc (fun r2 ->
        result3 |> Map (toCSharpFunc (fun r3 ->
          onSuccess.Invoke(r1, r2, r3)))))))

  let Map4 (onSuccess:Func<'a, 'b, 'c, 'd, 'e>) result1 result2 result3 result4 =
    result1 |> Bind (toCSharpFunc (fun r1 ->
      result2 |> Bind (toCSharpFunc (fun r2 ->
        result3 |> Bind (toCSharpFunc (fun r3 ->
          result4 |> Map (toCSharpFunc (fun r4 ->
            onSuccess.Invoke(r1, r2, r3, r4)))))))))

  let Success v = Success v
  let Failure v = Failure v
