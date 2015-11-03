package Dojo

import org.scalatest._
import Dojo.Main._

class PrimeSpec extends FunSpec with Matchers {

  it("test if 2 is prime") {
    assert (isPrime(2))
  }

}
