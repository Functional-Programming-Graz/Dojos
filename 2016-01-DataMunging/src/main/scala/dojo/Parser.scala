package dojo

import scala.io.Source

/**
  * The file football.dat contains the results from the English Premier League
  * for 2001/2. The columns labeled ‘F’ and ‘A’ contain the total number of goals s
  * cored for and against each team in that season (so Arsenal scored 79 goals
  * against opponents, and had 36 goals scored against them). Write a program to
  * print the name of the team with the smallest difference in ‘for’ and ‘against’ goals.
  */
trait Parser {
  type T <: {def spread: Int}
  val extractor: PartialFunction[String, T]
  def filename: String

  def readData(): T = {
    Source.fromFile(filename).getLines()
      .collect(extractor)
      .minBy(_.spread)
  }
}

object ParserF {
  def findMin[T](filename: String, extractor: PartialFunction[String, T])(minBy: T => Int): T = {
    Source.fromFile(filename).getLines()
      .collect(extractor)
      .minBy(minBy)
  }
}