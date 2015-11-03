package Dojo

object Main {
  val sieve : Stream[Int] = {
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

  def bigIntFrom(from: BigInt, step: BigInt): Stream[BigInt] = {
    from #:: bigIntFrom(from + step, step)
  }

  def isPrime(i:Int) : Boolean =
    sieve.takeWhile(_<=i).contains(i)

}
