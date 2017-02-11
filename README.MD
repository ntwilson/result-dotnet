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
    ? Result.Failure<double, string>("Cannot divide by 0!")
    : Result.Success<double, string>(numerator / denominator);
```
You could also use the C#6 `using static` feature to simplify the above to:
```C#
using ResultDotNet;
using static ResultDotNet.Result;
...
Result<double, string> divide(double numerator, double denominator) =>
  (denominator == 0)
    ? Failure<double, string>("Cannot divide by 0!")
    : Success<double, string>(numerator / denominator);
```

#### extracting the value of a Result<tVal, tErr>
The easiest way to get the result is to use the `Match()` member:
```C#
using ResultDotNet;
...
string pricePerUnitForDisplay(Invoice invoice) =>
  divide(invoice.Total, invoice.NumberOfUnits).Match(
    success: ppu => ppu.ToString(),
    failure: err => $"N/A: {err}");
```
Result is a union of types `Result<tVal, tErr>.Success` and `Result<tVal, tErr>.Failure` (`Result<tVal,tErr>` itself is abstract, and has the two unioned types as concrete child classes), so you can also manually check the types:
```C#
using ResultDotNet;
...
string pricePerUnitForDisplay(Invoice invoice) {
  var ans = divide(invoice.Total, invoice.NumberOfUnits);
  if (ans is Result<double, string>.Success) 
    return (ans as Result<double, string>.Success).Item.ToString();
  else {
    var err = (ans as Result<double, string>.Failure).Item;
    return $"N/A: {err}";
  }
}
```
Following the same idea, you could use a package for pattern matching, such as Ifp.PatternMatching, to write something like:
```C#
using ResultDotNet;
using PatternMatching;
...
string pricePerUnitForDisplay(Invoice invoice) =>
  Pattern.Match<Result<double, string>, string>(divide(invoice.Total, invoice.NumberOfUnits))
    .Case<Result<double, string>.Success>(div => div.Item.ToString())
    .Case<Result<double, string>.Failure>(err => $"N/A: {err.Item}");
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
Invoice createInvoice(double total, double numberOfUnits) => new Invoice(total, numberOfUnits)

Result<Invoice, string> createInvoice(Result<double, string> total, Result<double, string> numberOfUnits) =>
  Result.Map2(createInvoice, total, numberOfUnits);
```

#### taking actions on success or failure
if you need to take an action on success or failure instead of returning a value, you can use the overloads for the `Match()` member, or the `IfSuccess()` and `IfFailure()` members:
```C#
using ResultDotNet;
...
Result<DataTable, string> result = executeDatabaseQuery(sql);
result.IfFailure(err => logger.Log(err));
```

```C#
using ResultDotNet;
...
Result<DataTable, string> result = executeDatabaseQuery(sql);
result.Match(
  success: val => logger.Log($"DB query ran successfully: {sql}"),
  failure: err => logger.Log($"DB query FAILED: {sql}"));
```

### from F&#35;
Since Result uses many higher order functions, using the C# interface doesn't interop well with F# (since F# prefers `FSharpFunc`s instead of `System.Func`s).
To make usage from F# easier, there's a ResultDotNet.FSharp namespace that shadows the Result module with one that uses F#-friendly functions 
#### creating a Result<'tVal, 'tErr>
```F#
open ResultDotNet
...
let divide (numerator:float) (denominator:float) =
  if denominator = 0
  then Failure "Cannot divide by 0!"
  else Success (numerator / denominator)
```

#### extracting the value of a Result<'tVal, 'tErr>
Result is a union of types `Success of 'tVal` and `Failure of 'tErr`, so the easiest way to get the result is to use a `match` statement:
```F#
open ResultDotNet
...
let pricePerUnitForDisplay invoice =
  match divide invoice.Total invoice.NumberOfUnits with
  | Success ppu -> ppu.ToString()
  | Failure err -> "N/A: " + err
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
  pricePerUnit invoice |> Result.bind (fun ppu -> divide(dollarsOff, ppu))
  // you could of course `pricePerUnit () |> Result.bind (divide dollarsOff)`
  // but I find it counterintuitive that dollarsOff would be the numerator with that syntax

let pricePerUnitWithDiscount invoice dollarsOffPerUnit =
  pricePerUnit invoice |> result.map (fun ppu -> ppu - dollarsOffPerUnit)
```
```F#
open ResultDotNet
open ResultDotNet.FSharp
...
type Invoice = { Total:float; NumberOfUnits:float }
let createInvoice total numberOfUnits = { Total = total; NumberOfUnits = numberOfUnits }

let createInvoice (total:Result<double, string>) (numberOfUnits:Result<double, string>) =
  Result.map2 createInvoice total numberOfUnits
```

#### taking actions on success or failure
if you need to take an action on success or failure instead of returning a value, you can use the match statement as normal, or you can use the `ifSuccess` and `ifFailure` functions:
```F#
open ResultDotNet
open ResultDotNet.FSharp
...
let result:Result<DataTable, string> = executeDatabaseQuery sql
result |> Result.ifFailure (logger.Log);
```
