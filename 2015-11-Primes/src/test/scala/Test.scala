package Dojo

import org.scalatest._
import Dojo.Main._

object Test extends FunSuite{

  test("test if 2 is prime") {
    assert (isPrime(2))
  }

}