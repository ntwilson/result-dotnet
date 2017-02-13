namespace ResultDotNet

/// <summary>
/// Static functions for working with Result objects 
/// </summary>
module Result =
  open System
  open FuncTransforms
  open ResultDotNet.FSharp
  
  /// <summary>
  /// If the Result is success, "unwraps" the successful value and passes it
  /// to the function given, returning the result of that function.  If the 
  /// Result is failure, returns the failure without calling the function given. 
  /// </summary>
  let Bind onSuccess result = Result.bind (toFSharpFunc onSuccess) result

  /// <summary>
  /// If all the Results are success, "unwraps" the successful values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is failure, returns the first failure without calling the function given. 
  /// </summary>
  let Bind2 onSuccess result1 result2 = Result.bind2 (toFSharpFunc2 onSuccess) result1 result2

  /// <summary>
  /// If all the Results are success, "unwraps" the successful values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is failure, returns the first failure without calling the function given. 
  /// </summary>
  let Bind3 onSuccess result1 result2 result3 = 
    Result.bind3 (toFSharpFunc3 onSuccess) result1 result2 result3

  /// <summary>
  /// If all the Results are success, "unwraps" the successful values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is failure, returns the first failure without calling the function given. 
  /// </summary>
  let Bind4 onSuccess result1 result2 result3 result4 = 
    Result.bind4 (toFSharpFunc4 onSuccess) result1 result2 result3 result4

  /// <summary>
  /// If all the Results are success, "unwraps" the successful values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is failure, returns the first failure without calling the function given. 
  /// </summary>
  let BindAll onSuccess results = Result.bindAll (toFSharpFunc onSuccess) results

  /// <summary>
  /// If all the Results are success, "unwraps" the successful values and passes it
  /// to the function given, returning a Success with the result of that function.  
  /// If the Result is failure, returns the failure without calling the function given. 
  /// </summary>
  let Map onSuccess result = Result.map (toFSharpFunc onSuccess) result

  /// <summary>
  /// If the Result is success, "unwraps" the successful values and passes them
  /// to the function given, returning a Success with the result of that function.  
  /// If any Result is failure, returns the first failure without calling the function given. 
  /// </summary>
  let Map2 onSuccess result1 result2 = Result.map2 (toFSharpFunc2 onSuccess) result1 result2 

  /// <summary>
  /// If the Result is success, "unwraps" the successful values and passes them
  /// to the function given, returning a Success with the result of that function.  
  /// If any Result is failure, returns the first failure without calling the function given. 
  /// </summary>
  let Map3 onSuccess result1 result2 result3 = 
    Result.map3 (toFSharpFunc3 onSuccess) result1 result2 result3 

  /// <summary>
  /// If the Result is success, "unwraps" the successful values and passes them
  /// to the function given, returning a Success with the result of that function.  
  /// If any Result is failure, returns the first failure without calling the function given. 
  /// </summary>
  let Map4 onSuccess result1 result2 result3 result4 = 
    Result.map4 (toFSharpFunc4 onSuccess) result1 result2 result3 result4

  /// <summary>
  /// If the Result is success, "unwraps" the successful values and passes them
  /// to the function given, returning a Success with the result of that function.  
  /// If any Result is failure, returns the first failure without calling the function given. 
  /// </summary>
  let MapAll onSuccess results = Result.mapAll (toFSharpFunc onSuccess) results

  /// <summary>
  /// Creates a new successful Result with the value given 
  /// </summary>
  let Success<'tVal, 'tErr> v : Result<'tVal, 'tErr> = Success v

  /// <summary>
  /// Creates a new failure Result with the error object given 
  /// </summary>
  let Failure<'tVal, 'tErr> v : Result<'tVal, 'tErr> = Failure v
