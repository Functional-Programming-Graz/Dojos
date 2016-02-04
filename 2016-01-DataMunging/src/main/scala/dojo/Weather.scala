package dojo

import java.io.File

import scala.io.Source
import scala.util.Try

/**
  * Part One: Weather Data
In weather.dat you’ll find daily weather data for Morristown, NJ for June 2002.
Download this text file, then write a program to output the day number (column one)
with the smallest temperature spread (the maximum temperature is the second column,
the minimum the third column).
  */
object Weather extends Parser {
  type T = Data

  def filename = "src/resource/data/weather.dat"

  override val extractor: PartialFunction[String, T] = Function.unlift(
    { s =>
      Try {
        val row = s.split("[ *]+")
        Data(row(1).toInt, row(2).toInt, row(3).toInt) }.toOption
    }
  )

  /*
  override val extractor: PartialFunction[String, T] = {
        case RegEx(day, max, min) => Data(day.toInt, max.toInt, min.toInt)
  }
  val RegEx = " +(\\S+)[^0-9]+([0-9]+)[^0-9]+([0-9]+).*".r
*/
  case class Data(day: Int, maxTemp: Int, minTemp: Int) {
    def spread = maxTemp - minTemp
  }
}