package dojo

import java.nio.charset.StandardCharsets

import org.scalatest.{DiagrammedAssertions, FunSpec}

import scala.io.Source

class WordChainSpec extends FunSpec with DiagrammedAssertions {
  it("returns empty sequence for empty sequence") {
    assert (WordChain.calculate(Set("cot", "cat", "cog", "dog"))("cat", "dog") equals Some(List("cat", "cot", "cog", "dog")))
  }

  it("can handle large word lists") {
    val words = Source.fromFile("src/main/resources/wordlist.txt",
    StandardCharsets.ISO_8859_1.name())
      .getLines().toList
    assert(words.size == 338882)
    fail("naïve implementation cannot handle this")
    val result = WordChain.calculate(words.filter(_.size == 3).toSet)("cat", "dog")
    assert (result equals Some(List("cat", "cot", "cog", "dog")))
  }
}