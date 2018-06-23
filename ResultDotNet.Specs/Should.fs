namespace ResultDotNet.Specs

module Should = 
  open NUnit.Framework

  let shouldBe (expected:'a) (actual:'a) = 
    Assert.AreEqual (expected, actual)

  let shouldNotBe (expected:'a) (actual:'a) = 
    Assert.AreNotEqual (expected, actual)

  let shouldSatisfy predicate actual =
    predicate actual |> shouldBe true

  let shouldNotSatisfy predicate actual = 
    predicate actual |> shouldNotBe true