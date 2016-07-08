package dojo

import java.io.File
import java.nio.charset.StandardCharsets

import scala.io.Source

/**
  http://codekata.com/kata/kata19-word-chains/
  */
object WordChain {

  def findIt(wordsToNeighbors: Map[String, Set[String]], candidate: String, endWordOfLengthN: String, visited: Set[String]): Option[Seq[String]] = {
      val nextCandidates = wordsToNeighbors(candidate).filterNot(visited)
      if (nextCandidates.contains(endWordOfLengthN)) { Some(Seq(candidate, endWordOfLengthN)) }
      else if (nextCandidates.isEmpty) {None}
      else {
        nextCandidates.flatMap(findIt(wordsToNeighbors, _, endWordOfLengthN, visited ++ nextCandidates)) match {
          case xs if xs.isEmpty => None
          case xs => Some(candidate +: xs.minBy(_.length))
        }
      }
  }

  def calculate(wordsOfLengthN : Set[String])(startWordOfLengthN: String, endWordOfLengthN: String) : Option[Seq[String]] = {
    var wordsToNeighbors = Map[String, Set[String]]()

    wordsOfLengthN.foreach { word =>
      val neighbors = wordsOfLengthN.filter(areNeighbors(word, _))
      wordsToNeighbors += word -> neighbors
    }

    findIt(wordsToNeighbors, startWordOfLengthN, endWordOfLengthN, Set())
  }

  def areNeighbors(as: String, bs: String): Boolean = {
    as.length == bs.length &&
    hammingDistance(as, bs) == 1
  }

  def hammingDistance(as: String, bs: String): Int = {
    as.zip(bs).count { case (a, b) => a != b }
  }

  def main(args: Array[String]) {
    /*
    var mutableVariable = "a"
    mutableVariable = "b"

    val immutableValue = "a"
    //immutableValue  = "b"

    val words = Source.fromFile(new File("/Users/broberts/workspace/Dojos/2016-02-Anagrams/src/main/resources/wordlist.txt"),
      StandardCharsets.ISO_8859_1.name())
      .getLines().toList

    val anagrams = WordChain.calculate(words)

    anagrams.foreach { anagramSet =>
      println(anagramSet.mkString(","))
    }
    */
  }
}