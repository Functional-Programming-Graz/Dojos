package dojo

import org.scalatest.{DiagrammedAssertions, FunSpec}

/**
  * Part One: Weather Data
In weather.dat you’ll find daily weather data for Morristown, NJ for June 2002.
Download this text file, then write a program to output the day number (column one)
with the smallest temperature spread (the maximum temperature is the second column,
the minimum the third column).
  */
class WeatherSpec extends FunSpec with DiagrammedAssertions {
  it("outputs day of smallest spread") {
    assert(14 == Weather.readData().day)
  }
}