namespace ResultDotNet.Specs

module Should = 
  open Xunit

  let shouldBe (expected:'a) (actual:'a) = 
    Assert.Equal<'a> (expected, actual)

  let shouldNotBe (expected:'a) (actual:'a) = 
    Assert.NotEqual<'a> (expected, actual)

  let shouldSatisfy predicate actual =
    predicate actual |> shouldBe true

  let shouldNotSatisfy predicate actual = 
    predicate actual |> shouldNotBe true