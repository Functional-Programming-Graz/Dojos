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

  it("5 is sum of consecutive primes") {
    assert(isSumOfConsecutivePrimes(5, 2))
  }

  it("11 is not sum of consecutive primes") {
    assert(!isSumOfConsecutivePrimes(11, 4))
  }

  it("17 is sum of consecutive primes") {
    assert(isSumOfConsecutivePrimes(17, 6))
  }

  it("953 is sum") {
    assert(isSumOfConsecutivePrimes(953, sieve.indexOf(953)))
  }

  it("The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.") {
    val prime = sumOfPrimes.takeWhile(_._1 < 1000).maxBy(_._2.size)
    assert(prime._2.sum == prime._1)
    assert(prime._1 == 953)
  }

  it("finds count of primes to 1 million") {
    println("Count of primes 1m: "+ sieve.takeWhile(_ < 1000000).size)
  }
  /*
  it("finds longest sum of consecutive primes below 1 million that adds to a prime") {
    val prime = sumOfPrimes.takeWhile(_._1 < 1000000).maxBy(_._2.size)
    assert(prime._2.sum == prime._1)
    assert(prime._1 == 953)
  }
  */
  /*
  it("can it do max int?") {
    val largeNumber = Int.MaxValue / 2
    println("Trying to find a big prime larger than: " + largeNumber)
    val prime = sieveBig.dropWhile(_ < largeNumber).head
    println("A prime: " + prime)
  }
  */

}
