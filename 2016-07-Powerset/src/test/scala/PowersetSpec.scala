import org.scalatest._

class PowersetSpec extends FunSpec with Matchers {
  it("powerset of empty set is the set of empty set") {
    Powerset(Stream.empty) shouldBe Stream(Stream.empty)
  }

  it("powerset of single element stream is the set of empty set and the set") {
    Powerset(Stream(1)) shouldBe Stream(Stream.empty, Stream(1))
  }

  it("powerset of two element stream is the set of empty set and ...") {
    Powerset(Stream(1, 2)) shouldBe Stream(
      Stream.empty,
      Stream(1),
      Stream(2),
      Stream(2, 1))
  }

  it("powerset of three element stream is the set of empty set and ...") {
    Powerset(Stream(1, 2, 3)) shouldBe Stream(
      Stream.empty,
      Stream(1),
      Stream(2),
      Stream(2, 1),
      Stream(3),
      Stream(3, 1),
      Stream(3, 2),
      Stream(3, 2, 1)
    )
  }

  it("Can handle huge streams") {
    Powerset(Stream.range(1, 10)).length shouldBe (Math.pow(2, 9).toInt)
  }

  it("Can handle infinite streams") {
    Powerset(Stream.from(1)).take(1024).last shouldBe Stream(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    Powerset(Stream.from(1)).take(1024*1024).last.length shouldBe 20
  }
}
