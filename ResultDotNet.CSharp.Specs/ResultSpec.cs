using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using ResultDotNet;
using static ResultDotNet.Result;

namespace ResultDotNet.CSharp.Specs {
    [TestClass]
    public class ResultSpec {

        [TestMethod]
        public void MatchMemberShouldReturnAnAppropriateValue() {
            Ok<int, string>(5).Match(
                ok: i => i.ToString(),
                error: err => err)
            .ShouldBe("5");
            
            Error<int, string>("didn't work").Match(
                ok: i => i.ToString(),
                error: err => err)
            .ShouldBe("didn't work");
        }

        [TestMethod]
        public void MatchMemberShouldSupportActions() {
            var didRun = false;
            Ok<int, string>(5).Match(
                ok: i => { didRun = true; },
                error: err => { });

            didRun.ShouldBe(true);

            didRun = false;

            Error<int, string>("didn't work").Match(
                ok: i => {},
                error: err => { didRun = true; });

            didRun.ShouldBe(true);
        }

        [TestMethod]
        public void HasMembersToExecuteActionsSpecificallyForErrorsOrOkes() {
            var didRun = false;
            Ok<int, string>(5).IfOk(i => { didRun = true; });
            didRun.ShouldBe(true);

            didRun = false;
            Error<int, string>("didn't work").IfOk(i => { didRun = true; });
            didRun.ShouldBe(false);

            didRun = false;
            Error<int, string>("didn't work").IfError(err => { didRun = true; });
            didRun.ShouldBe(true);

            didRun = false;
            Ok<int, string>(5).IfError(err => { didRun = true; });
            didRun.ShouldBe(false);
        }

        [TestMethod]
        public void MapMemberShouldWorkWithNonResults() {
            Ok<int, string>(5).Map(i => i + 3)
            .ShouldBe(Ok<int, string>(8));

            Error<int, string>("didn't work").Map(i => i + 3)
            .ShouldBe(Error<int, string>("didn't work"));
        }

        [TestMethod]
        public void BindMemberShouldWorkWithResultFuncs() {
            Ok<int, string>(5).Bind(i => Ok<int, string>(i + 3))
            .ShouldBe(Ok<int,string>(8));

            Error<int, string>("didn't work").Bind(i => Ok<int, string>(i + 3))
            .ShouldBe(Error<int, string>("didn't work"));
        }

        [TestMethod]
        public void StaticBindAndMapMembersPassThroughToFSharpVersions() {
            Result.Map2((int a, string b) => a.ToString() + b,
                Ok<int, string>(99),
                Ok<string, string>(" bottles of beer on the wall"))
            .ShouldBe(Ok<string, string>("99 bottles of beer on the wall"));

            Result.Bind3((int a, string b, string c) => 
                    Ok<string, string>(a.ToString() + b + ". " + a.ToString() + c),
                Ok<int, string>(99),
                Ok<string, string>(" bottles of beer on the wall"),
                Ok<string, string>(" bottles of beer"))
            .ShouldBe(Ok<string, string>("99 bottles of beer on the wall. 99 bottles of beer"));
        }
    }
}
