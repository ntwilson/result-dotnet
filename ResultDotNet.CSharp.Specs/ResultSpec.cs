using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using ResultDotNet;
using static ResultDotNet.Result;

namespace ResultDotNet.CSharp.Specs {
    [TestClass]
    public class ResultSpec {

        [TestMethod]
        public void MatchMemberShouldReturnAnAppropriateValue() {
            Success<int, string>(5).Match(
                success: i => i.ToString(),
                failure: err => err)
            .ShouldBe("5");
            
            Failure<int, string>("didn't work").Match(
                success: i => i.ToString(),
                failure: err => err)
            .ShouldBe("didn't work");
        }

        [TestMethod]
        public void MatchMemberShouldSupportActions() {
            var didRun = false;
            Success<int, string>(5).Match(
                success: i => { didRun = true; },
                failure: err => { });

            didRun.ShouldBe(true);

            didRun = false;

            Failure<int, string>("didn't work").Match(
                success: i => {},
                failure: err => { didRun = true; });

            didRun.ShouldBe(true);
        }

        [TestMethod]
        public void HasMembersToExecuteActionsSpecificallyForFailuresOrSuccesses() {
            var didRun = false;
            Success<int, string>(5).IfSuccess(i => { didRun = true; });
            didRun.ShouldBe(true);

            didRun = false;
            Failure<int, string>("didn't work").IfSuccess(i => { didRun = true; });
            didRun.ShouldBe(false);

            didRun = false;
            Failure<int, string>("didn't work").IfFailure(err => { didRun = true; });
            didRun.ShouldBe(true);

            didRun = false;
            Success<int, string>(5).IfFailure(err => { didRun = true; });
            didRun.ShouldBe(false);
        }

        [TestMethod]
        public void MapMemberShouldWorkWithNonResults() {
            Success<int, string>(5).Map(i => i + 3)
            .ShouldBe(Success<int, string>(8));

            Failure<int, string>("didn't work").Map(i => i + 3)
            .ShouldBe(Failure<int, string>("didn't work"));
        }

        [TestMethod]
        public void BindMemberShouldWorkWithResultFuncs() {
            Success<int, string>(5).Bind(i => Success<int, string>(i + 3))
            .ShouldBe(Success<int,string>(8));

            Failure<int, string>("didn't work").Bind(i => Success<int, string>(i + 3))
            .ShouldBe(Failure<int, string>("didn't work"));
        }

        [TestMethod]
        public void StaticBindAndMapMembersPassThroughToFSharpVersions() {
            Result.Map2((int a, string b) => a.ToString() + b,
                Success<int, string>(99),
                Success<string, string>(" bottles of beer on the wall"))
            .ShouldBe(Success<string, string>("99 bottles of beer on the wall"));

            Result.Bind3((int a, string b, string c) => 
                    Success<string, string>(a.ToString() + b + ". " + a.ToString() + c),
                Success<int, string>(99),
                Success<string, string>(" bottles of beer on the wall"),
                Success<string, string>(" bottles of beer"))
            .ShouldBe(Success<string, string>("99 bottles of beer on the wall. 99 bottles of beer"));
        }
    }
}
