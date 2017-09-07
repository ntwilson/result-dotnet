# ResultDotNet
This library adds a Result class for FP-style error handling in C#.

## namespace overview
There are two classes in the ResultDotNet namespace: a `Result<tVal, tErr>` generic data type, and a `Result` static class.  The former is a data type that can be used to model values that might come back as an error, along with members to consume that data.  The latter is a set of static methods that work on the Result data type that don't read well as members - notably Map2 through Map4 and Bind2 through Bind4 - as well as functions for creating new Result data types.

## usage
### from C&#35;
#### creating a Result<tVal, tErr>
```C#
using ResultDotNet;
...
Result<double, string> divide(double numerator, double denominator) =>
  (denominator == 0)
    ? Result.Error<double, string>("Cannot divide by 0!")
    : Result.Ok<double, string>(numerator / denominator);
```
You could also use the C#6 `using static` feature to simplify the above to:
```C#
using ResultDotNet;
using static ResultDotNet.Result;
...
Result<double, string> divide(double numerator, double denominator) =>
  (denominator == 0)
    ? Error<double, string>("Cannot divide by 0!")
    : Ok<double, string>(numerator / denominator);
```

#### extracting the value of a Result<tVal, tErr>
The easiest way to get the result is to use the `Match()` member:
```C#
using ResultDotNet;
...
string pricePerUnitForDisplay(Invoice invoice) =>
  divide(invoice.Total, invoice.NumberOfUnits).Match(
    ok: ppu => ppu.ToString(),
    error: err => $"N/A: {err}");
```
Result is a union of types `Result<tVal, tErr>.Ok` and `Result<tVal, tErr>.Error` (`Result<tVal,tErr>` itself is abstract, and has the two unioned types as concrete child classes), so you can also manually check the types:
```C#
using ResultDotNet;
...
string pricePerUnitForDisplay(Invoice invoice) {
  var ans = divide(invoice.Total, invoice.NumberOfUnits);
  if (ans is Result<double, string>.Ok) 
    return (ans as Result<double, string>.Ok).Item.ToString();
  else {
    var err = (ans as Result<double, string>.Error).Item;
    return $"N/A: {err}";
  }
}
```
Following the same idea, you could use pattern matching from C# 7, to write something like:
```C#
using ResultDotNet;
...
string pricePerUnitForDisplay(Invoice invoice) =>
  var ans = divide(invoice.Total, invoice.NumberOfUnits);
  if (ans is Result<double,string>.Ok div) 
    return div.Item.ToString();
  else if (ans is Result<double,string>.Error err)
    return $"N/A: {err.Item}";
  else ...
```

#### map and bind as members
(I apologize for the *totally* contrived examples)
```C#
using ResultDotNet;
...
Result<double, string> pricePerUnit(Invoice invoice) => divide(invoice.Total, invoice.NumberOfUnits);
Result<double, string> savingsPerUnit(Invoice invoice, double dollarsOff) =>
  pricePerUnit(invoice).Bind(ppu => divide(dollarsOff, ppu));

Result<double, string> pricePerUnitWithDiscount(Invoice invoice, double dollarsOffPerUnit) =>
  pricePerUnit(invoice).Map(ppu => ppu - dollarsOffPerUnit);
```
You can also use LINQ to build expressions using Results.  You can think of the Result a bit like a collection that contains the successful result when assembling a LINQ expression. It can often be more intuitive and readable, at the expense of being slightly more total code:
```C#
using ResultDotNet;
...
Result<double, string> pricePerUnit(Invoice invoice) => divide(invoice.Total, invoice.NumberOfUnits);
Result<double, string> savingsPerUnit(Invoice invoice, double dollarsOff) =>
  from ppu in pricePerUnit(invoice) 
  from spu in divide(dollarsOff, ppu)
  select spu;

Result<double, string> pricePerUnitWithDiscount(Invoice invoice, double dollarsOffPerUnit) =>
  from ppu in pricePerUnit(invoice) select ppu - dollarsOffPerUnit;
``` 

#### map and bind as functions
map and bind themselves have static functions:
```C#
using ResultDotNet;
...
Result<double, string> pricePerUnit(Invoice invoice) => divide(invoice.Total, invoice.NumberOfUnits);
Result<double, string> savingsPerUnit(Invoice invoice, double dollarsOff) =>
  Result.Bind(ppu => divide(dollarsOff, ppu),  pricePerUnit(invoice));

Result<double, string> pricePerUnitWithDiscount(Invoice invoice, double dollarsOffPerUnit) =>
  Result.Map(ppu => ppu - dollarsOffPerUnit, pricePerUnit(invoice));
```
but there are also functions for Map2 through Map4 and Bind2 through Bind4 that only exist as static functions (object methods are hard to read when binding or mapping with multiple Results)
```C#
using ResultDotNet;
...
Invoice createInvoice(double total, double numberOfUnits) => new Invoice(total, numberOfUnits);

Result<Invoice, string> createInvoice(Result<double, string> total, Result<double, string> numberOfUnits) =>
  Result.Map2(createInvoice, total, numberOfUnits);
```

#### taking actions on ok or error
if you need to take an action on ok or error instead of returning a value, you can use the overloads for the `Match()` member, or the `IfOk()` and `IfError()` members:
```C#
using ResultDotNet;
...
Result<DataTable, string> result = executeDatabaseQuery(sql);
result.IfError(err => logger.Log(err));
```

```C#
using ResultDotNet;
...
Result<DataTable, string> result = executeDatabaseQuery(sql);
result.Match(
  ok: val => logger.Log($"DB query ran successfully: {sql}"),
  error: err => logger.Log($"DB query FAILED: {sql}"));
```

### from F&#35;
Since Result uses many higher order functions, using the C# interface doesn't interop well with F# (since F# prefers `FSharpFunc`s instead of `System.Func`s).
To make usage from F# easier, there's a ResultDotNet.FSharp namespace that shadows the Result module with one that uses F#-friendly functions 
#### creating a Result<'tVal, 'tErr>
```F#
open ResultDotNet
...
let divide (numerator:float) (denominator:float) =
  if denominator = 0.
  then Error "Cannot divide by 0!"
  else Ok (numerator / denominator)
```

#### extracting the value of a Result<'tVal, 'tErr>
Result is a union of types `Ok of 'tVal` and `Error of 'tErr`, so the easiest way to get the result is to use a `match` statement:
```F#
open ResultDotNet
...
let pricePerUnitForDisplay invoice =
  match divide invoice.Total invoice.NumberOfUnits with
  | Ok ppu -> ppu.ToString()
  | Error err -> "N/A: " + err
```

#### map and bind as members
(I apologize for the *totally* contrived examples)
```F#
open ResultDotNet
open ResultDotNet.FSharp
...
type Invoice = { Total:float; NumberOfUnits:float }
let pricePerUnit invoice = divide invoice.Total invoice.NumberOfUnits
let savingsPerUnit invoice dollarsOff =
  pricePerUnit invoice |> Result.bind (fun ppu -> divide dollarsOff ppu)
  // you could of course `pricePerUnit () |> Result.bind (divide dollarsOff)`
  // but I find it counterintuitive that dollarsOff would be the numerator with that syntax

let pricePerUnitWithDiscount invoice dollarsOffPerUnit =
  pricePerUnit invoice |> Result.map (fun ppu -> ppu - dollarsOffPerUnit)
```
```F#
open ResultDotNet
open ResultDotNet.FSharp
...
type Invoice = { Total:float; NumberOfUnits:float }
let newInvoice total numberOfUnits = { Total = total; NumberOfUnits = numberOfUnits }

let createInvoice (total:Result<double, string>) (numberOfUnits:Result<double, string>) =
  Result.map2 newInvoice total numberOfUnits
```

#### taking actions on ok or error
if you need to take an action on ok or error instead of returning a value, you can use the match statement as normal, or you can use the `ifOk` and `ifError` functions:
```F#
open ResultDotNet
open ResultDotNet.FSharp
...
let result:Result<DataTable, string> = executeDatabaseQuery sql
result |> Result.ifError (logger.Log);
```

#### computation expressions
when using ResultDotNet from F#, you can use a computation expression in place of bind & map.  These are repeats of the above examples now using computation expressions:
```F#
open ResultDotNet
open ResultDotNet.FSharp
...
type Invoice = { Total:float; NumberOfUnits:float }
let pricePerUnit invoice = divide invoice.Total invoice.NumberOfUnits
let savingsPerUnit invoice dollarsOff =
  Result.expr {
    let! ppu = pricePerUnit invoice
    return! divide dollarsOff ppu
  }

let pricePerUnitWithDiscount invoice dollarsOffPerUnit =
  Result.expr {
    let! ppu = pricePerUnit invoice
    return ppu - dollarsOffPerUnit
  }
```
```F#
open ResultDotNet
open ResultDotNet.FSharp
...
type Invoice = { Total:float; NumberOfUnits:float }
let newInvoice total numberOfUnits = { Total = total; NumberOfUnits = numberOfUnits }

let createInvoice (total:Result<double, string>) (numberOfUnits:Result<double, string>) =
  Result.expr {
    let! t = total
    let! n = numberOfUnits
    return newInvoice t n
  }
```
