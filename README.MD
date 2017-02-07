This library adds a Result class for FP-style error handling in C#.

# namespace overview
There are two classes in the ResultDotNet namespace: a `Result<tVal, tErr>` generic data type, and a `Result` static class.  The former is a data type that can be used to model values that might come back as an error, along with members to consume that data.  The latter is a set of static methods that work on the Result data type that don't read well as members - notably Map2 through Map4 and Bind2 through Bind4 - as well as functions for creating new Result data types.

# usage
## creating a Result<tVal, tErr>
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

## extracting the value of a Result<tVal, tErr>
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

## map and bind as members
(I apologize for the *totally* contrived examples)
```C#
using ResultDotNet;
...
Result<double, string> pricePerUnit(Invoice invoice) => divide(invoice.Total, invoice.NumberOfUnits);
Result<double, string> savingsPerUnit(Invoice invoice, double dollarsOff) =>
  pricePerUnit().Bind(ppu => divide(dollarsOff, ppu));

Result<double, string> pricePerUnitWithDiscount(Invoice invoice, double dollarsOffPerUnit) =>
  pricePerUnit().Map(ppu => ppu - dollarsOffPerUnit);
```

## map and bind as functions
map and bind themselves have static functions:
```C#
using ResultDotNet;
...
Result<double, string> pricePerUnit(Invoice invoice) => divide(invoice.Total, invoice.NumberOfUnits);
Result<double, string> savingsPerUnit(Invoice invoice, double dollarsOff) =>
  Result.Bind(ppu => divide(dollarsOff, ppu),  pricePerUnit());

Result<double, string> pricePerUnitWithDiscount(Invoice invoice, double dollarsOffPerUnit) =>
  Result.Map(ppu => ppu - dollarsOffPerUnit, pricePerUnit());
```
but there are also functions for Map2 through Map4 and Bind2 through Bind4 that only exist as static functions (object methods are hard to read when binding or mapping with multiple Results)
```C#
using ResultDotNet;
...
Invoice createInvoice(double total, double numberOfUnits) => new Invoice(total, numberOfUnits)

Result<Invoice, string> createInvoice(Result<double, string> total, Result<double, string> numberOfUnits) =>
  Result.Map2(createInvoice, total, numberOfUnits);
```
