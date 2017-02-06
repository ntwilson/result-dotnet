namespace ResultDotNet.Specs

module Should = 
  open NUnit.Framework

  let shouldBe expected actual = 
    Assert.AreEqual (expected, actual)

  let shouldNotBe expected actual = 
    Assert.AreNotEqual (expected, actual)

  let shouldSatisfy predicate actual =
    predicate actual |> shouldBe true

  let shouldNotSatisfo predicate actual = 
    predicate actual |> shouldNotBe true