namespace ResultDotNet.FSharp

open ResultDotNet
open FuncTransforms

type ResultExpression () =
  member this.Bind (x:Result<_,_>, onSuccess) = x.Bind (toCSharpFunc onSuccess)
  member this.Return x = Success x
  member this.ReturnFrom x = x

module Result =
  open System
  
  let expr = new ResultExpression ()

  let bind (onSuccess : 'tVal -> Result<'a, 'tErr>) (result:Result<'tVal, 'tErr>) = 
    result.Bind (toCSharpFunc onSuccess)

  let bind2 onSuccess result1 result2 =
    result1 |> bind (fun r1 -> 
      result2 |> bind (fun r2 -> 
        onSuccess r1 r2))

  let bind3 onSuccess result1 result2 result3 =
    result1 |> bind (fun r1 ->
      result2 |> bind (fun r2 -> 
        result3 |> bind (fun r3 ->
          onSuccess r1 r2 r3)))

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

  let bindAll onSuccess results = 
    concatResults results
    |> bind onSuccess

  let map (onSuccess : 'tVal -> 'a) (result:Result<'tVal, 'tErr>) = 
    result.Map (toCSharpFunc onSuccess)

  let map2 onSuccess result1 result2 = 
    result1 |> bind (fun r1 ->
      result2 |> map (fun r2 ->
        onSuccess r1 r2))

  let map3 onSuccess result1 result2 result3 = 
    result1 |> bind (fun r1 ->
      result2 |> bind (fun r2 ->
        result3 |> map (fun r3 ->
          onSuccess r1 r2 r3)))

  let map4 onSuccess result1 result2 result3 result4 =
    result1 |> bind (fun r1 ->
      result2 |> bind (fun r2 ->
        result3 |> bind (fun r3 ->
          result4 |> map (fun r4 ->
            onSuccess r1 r2 r3 r4))))
          
  let mapAll onSuccess results = 
    concatResults results
    |> map onSuccess

  let ifSuccess success (result:Result<_,_>) = result.IfSuccess (Action<_> success)

  let ifFailure failure (result:Result<_,_>) = result.IfFailure (Action<_> failure)