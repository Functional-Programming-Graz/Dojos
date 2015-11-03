package Dojo

object Main {
  def find44thPrime : Int = ???

  def sieve : Stream[Int] = Stream(2)

  def isPrime(i:Int) : Boolean =
    sieve.takeWhile(_<=i).contains(i)


  def main (args: Array[String]) {
    ???
  }





}
