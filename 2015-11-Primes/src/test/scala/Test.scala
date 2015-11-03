package Dojo

import org.scalatest._
import Dojo.Main._

class PrimeSpec extends FunSpec with Matchers {

  it("test if 2 is prime") {
    assert (isPrime(2))
  }

  it("4 is not a prime") {
    assert(!isPrime(4))
  }

  it("5 is a prime") {
    assert(isPrime(5))
  }

  it("the 44th prime after 100,000 exists") {
    pending
  }

}
