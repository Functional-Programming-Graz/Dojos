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

  it("9 is not a prime") {
    assert(!isPrime(9))
  }

  it("13 is a prime") {
    assert(isPrime(13))
  }

  it("the 44th prime after 100,000 exists") {
    val thePrime = sieve.dropWhile(_ <= 100000)
      .drop(43)
      .head

    println("The prime is: " + thePrime)
    assert(thePrime > 100000)
  }

  it("bonus: the next four primes after the 44th prime after 100,000") {
    val theNextPrimes = sieve.dropWhile(_ <= 100000)
      .drop(43)
      .drop(1)
      .take(4)
      .toList

    println("The next primes are: " + theNextPrimes)
    assert(theNextPrimes.size == 4)
  }

  /*
  it("can it do max int?") {
    val largeNumber = Int.MaxValue / 2
    println("Trying to find a big prime larger than: " + largeNumber)
    val prime = sieveBig.dropWhile(_ < largeNumber).head
    println("A prime: " + prime)
  }
  */

}
