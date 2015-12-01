package Dojo

object Main {
  val sieve : Stream[Int] = {
    // Note: filterNot is broken in 2.11.7, see https://issues.scala-lang.org/browse/SI-8627
    2 #:: Stream.from(3, 2).filter { i =>
      !sieve.takeWhile(j => j * j <= i)
            .exists(j => i % j == 0)
    }
  }

  val sieveBig : Stream[BigInt] = {
    BigInt(2) #:: bigIntFrom(3, 2).filter { i =>
      !sieveBig.takeWhile(j => j * j <= i)
               .exists(j => i % j == 0)
    }
  }

  /*
  def intermediateSums(indexOfPrime: Int): Array[Int] = {

  }
  */

  def sumOfPrimes: Stream[(Int, List[Int])] = {
    sieve.zipWithIndex.flatMap {case (prime, indexOfPrime) =>
        sumOfConsecutivePrimes(prime, indexOfPrime)
          .map(prime -> _)
    }
  }

  def sumOfConsecutivePrimes(prime: Int, indexOfPrime: Int): Option[List[Int]] = {
    Stream.from(0).takeWhile(_ <= indexOfPrime).flatMap { a =>
      Stream.from(2)
        .takeWhile(_ + a <= indexOfPrime)
        .map { b => sieve.drop(a).take(b) }
        .takeWhile(_.sum <= prime)
        .filter { values =>
        values.sum == prime
      }
    }.map(_.toList)
      .headOption
  }

  def isSumOfConsecutivePrimes(prime: Int, indexOfPrime: Int): Boolean = {
      sumOfConsecutivePrimes(prime, indexOfPrime).nonEmpty
/*    Stream.from(0).takeWhile(_ <= indexOfPrime).exists { a =>
      println("a = " + a)
      Stream.from(2)
        .takeWhile(_ + a <= indexOfPrime)
        .map { b => sieve.drop(a).take(b) }
        .takeWhile(_.sum <= prime)
        .exists { values =>
          println(prime + " testing: " + values.toList)
          values.sum == prime
      }
    }
    */
  }

  /**
The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13 This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
    3 + ..... n
    999,999,999

    2 + 3 + .... (n - 1)
    999,9??,???
   */
  def bigIntFrom(from: BigInt, step: BigInt): Stream[BigInt] = {
    from #:: bigIntFrom(from + step, step)
  }

  def isPrime(i:Int) : Boolean =
    sieve.takeWhile(_<=i).contains(i)

}
