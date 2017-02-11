namespace ResultDotNet

module Result =
  open System
  open FuncTransforms
  open ResultDotNet.FSharp
  
  let Bind onSuccess result = Result.bind (toFSharpFunc onSuccess) result

  let Bind2 onSuccess result1 result2 = Result.bind2 (toFSharpFunc2 onSuccess) result1 result2

  let Bind3 onSuccess result1 result2 result3 = 
    Result.bind3 (toFSharpFunc3 onSuccess) result1 result2 result3

  let Bind4 onSuccess result1 result2 result3 result4 = 
    Result.bind4 (toFSharpFunc4 onSuccess) result1 result2 result3 result4

  let BindAll onSuccess results = Result.bindAll (toFSharpFunc onSuccess) results

  let Map onSuccess result = Result.map (toFSharpFunc onSuccess) result

  let Map2 onSuccess result1 result2 = Result.map2 (toFSharpFunc2 onSuccess) result1 result2 

  let Map3 onSuccess result1 result2 result3 = 
    Result.map3 (toFSharpFunc3 onSuccess) result1 result2 result3 

  let Map4 onSuccess result1 result2 result3 result4 = 
    Result.map4 (toFSharpFunc4 onSuccess) result1 result2 result3 result4

  let MapAll onSuccess results = Result.mapAll (toFSharpFunc onSuccess) results

  let Success<'tVal, 'tErr> v : Result<'tVal, 'tErr> = Success v
  let Failure<'tVal, 'tErr> v : Result<'tVal, 'tErr> = Failure v
