using System;
using System.Data;
using NUnit.Framework;
using PatternMatching;
using static LanguageExt.Prelude;
using ResultDotNet;
using static ResultDotNet.Result;

namespace ResultDotNet.CSharp.Specs {
  /// <summary>
  /// this just makes sure that everything in the README compiles
  /// </summary>
  public class ReadmeSpec {
    class Invoice { public double Total; public double NumberOfUnits; }

    Result<double, string> divide(double numerator, double denominator) =>
      (denominator == 0)
        ? Failure<double, string>("Cannot divide by 0!")
        : Success<double, string>(numerator / denominator);

    string pricePerUnitForDisplay(Invoice invoice) =>
      divide(invoice.Total, invoice.NumberOfUnits).Match(
        success: ppu => ppu.ToString(),
        failure: err => $"N/A: {err}");
    
    string pricePerUnitForDisplay2(Invoice invoice) {
      var ans = divide(invoice.Total, invoice.NumberOfUnits);
      if (ans is Result<double, string>.Success) 
        return (ans as Result<double, string>.Success).Item.ToString();
      else {
        var err = (ans as Result<double, string>.Failure).Item;
        return $"N/A: {err}";
      }
    }
    
    string pricePerUnitForDisplay3(Invoice invoice) =>
      Pattern.Match<Result<double, string>, string>(divide(invoice.Total, invoice.NumberOfUnits))
        .Case<Result<double, string>.Success>(div => div.Item.ToString())
        .Case<Result<double, string>.Failure>(err => $"N/A: {err.Item}");

    Result<double, string> pricePerUnit(Invoice invoice) => divide(invoice.Total, invoice.NumberOfUnits);
    Result<double, string> savingsPerUnit(Invoice invoice, double dollarsOff) =>
      pricePerUnit(invoice).Bind(ppu => divide(dollarsOff, ppu));

    Result<double, string> pricePerUnitWithDiscount(Invoice invoice, double dollarsOffPerUnit) =>
      pricePerUnit(invoice).Map(ppu => ppu - dollarsOffPerUnit);
    
    Result<double, string> savingsPerUnit2(Invoice invoice, double dollarsOff) =>
      Result.Bind(ppu => divide(dollarsOff, ppu),  pricePerUnit(invoice));

    Result<double, string> pricePerUnitWithDiscount2(Invoice invoice, double dollarsOffPerUnit) =>
      Result.Map(ppu => ppu - dollarsOffPerUnit, pricePerUnit(invoice));
    
    Invoice createInvoice(double total, double numberOfUnits) => 
      new Invoice() { Total = total, NumberOfUnits = numberOfUnits };

    Result<Invoice, string> createInvoice(Result<double, string> total, Result<double, string> numberOfUnits) =>
      Result.Map2(createInvoice, total, numberOfUnits);

    Result<DataTable, string> executeDatabaseQuery(string sql) => 
      Failure<DataTable, string>("no database");

    public void Test() {
      var sql = "";
      var logger = new { Log = act((string err) => {}) };
      Result<DataTable, string> result = executeDatabaseQuery(sql);
      result.IfFailure(err => logger.Log(err));

      result.Match(
        success: val => logger.Log($"DB query ran successfully: {sql}"),
        failure: err => logger.Log($"DB query FAILED: {sql}"));
    }
  }
}