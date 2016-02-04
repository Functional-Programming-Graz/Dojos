package dojo

import java.io.File
import java.nio.charset.StandardCharsets

import org.scalatest.{DiagrammedAssertions, FunSpec}

import scala.io.Source

class AnagamSpec extends FunSpec with DiagrammedAssertions {
  it("returns empty sequence for empty sequence") {
    assert (Anagram.calculate(Seq()) equals Set())
  }

  it("returns empty seq for inputs that have no matches") {
    assert (Anagram.calculate(Seq("a", "b", "c")) equals Set())
  }

  it("returns match for trivial anagram") {
    assert (Anagram.calculate(Seq("ab", "ba")) equals Set(Set("ab", "ba")))
  }

  it("returns match for non-trivial anagram") {
    assert (Anagram.calculate(Seq("kinship", "pinkish", "enlist", "inlets", "listen", "silent",
      "boaster", "boaters", "borates",
      "fresher", "refresh",
      "sinks", "skins",
      "knits", "stink",
      "rots", "sort", "otherwise")) equals Set(Set("kinship", "pinkish"),
      Set("enlist", "inlets", "listen", "silent"),
      Set("boaster", "boaters", "borates"),
      Set("fresher", "refresh"),
      Set("sinks", "skins"),
      Set("knits", "stink"),
      Set("rots", "sort")))
  }

  it("can handle large word lists") {
    val words = Source.fromFile(new File("/Users/broberts/workspace/Dojos/2016-02-Anagrams/src/main/resources/wordlist.txt"),
    StandardCharsets.ISO_8859_1.name())
      .getLines().toList
    assert(words.size == 338882)
    val anagrams = Anagram.calculate(words)
    assert(anagrams.size == 20683)
    assert(anagrams.toList.map(_.size).sum == 48162)
  }
}