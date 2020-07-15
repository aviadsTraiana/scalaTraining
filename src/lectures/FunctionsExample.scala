package lectures

import scala.annotation.tailrec

object FunctionsExample extends App{
  def factorial(n: Int): Int = {
    if(n<=1) 1
    else factorial(n-1)*n
  }
  def fibonacci(n:Int):Int = if(n<=2) 1 else {
    fibonacci(n - 1) + fibonacci(n - 2)
  }
  def isPrime(x:Int) :Boolean = {
    @tailrec
    def isPrimeUntil(y:Int): Boolean = if(y<=1) true else {
      x % y != 0 && isPrimeUntil(y - 1)
    }
    isPrimeUntil(x/2)
  }
  println(isPrime(5))
  println(isPrime(5*2))

  def callByValue(x: Long) :Unit = {
    println(s"by value $x") //after x was evaluated by call
    println(s"by value $x") //after x was evaluated by call
  }
  def callByName(x: ⇒ Long): Unit ={
    println(s"by name $x") //System.nanoTime()
    println(s"by name $x")//System.nanoTime()
  }
  callByValue(System.nanoTime())
  callByValue(System.nanoTime())
  callByName(System.nanoTime())
  callByName(System.nanoTime())

  //curried function
  def curried = (x:Int) ⇒ (y:Int) ⇒ x+y
  println(curried(2)(3)) //2+3=5

  def composeNtimes[D](f:D⇒D, n:Int):D⇒D ={
    if(n<=0) (id:D)⇒ id
    else (x:D) ⇒ composeNtimes(f,n-1)(f(x))
  }
  // p=p^2
  def isIdempotent[D](p:D⇒D): Boolean = composeNtimes(p,2)==composeNtimes(p,1)


}
