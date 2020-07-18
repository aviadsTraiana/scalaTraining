package excercies

abstract sealed class MaybeMonad[+T] {

  def map[B](f: T⇒ B  ) :MaybeMonad[B]
  def flatMap[B](f: T⇒MaybeMonad[B]) : MaybeMonad[B]
  def filter(predicate: T⇒ Boolean) : MaybeMonad[T]
}

case object MaybeNot extends MaybeMonad[Nothing]{
  override def map[B](f: Nothing ⇒ B): MaybeMonad[B] = MaybeNot

  override def flatMap[B](f: Nothing ⇒ MaybeMonad[B]): MaybeMonad[B] = MaybeNot

  override def filter(predicate: Nothing ⇒ Boolean): MaybeMonad[Nothing] = MaybeNot
}

case class SomeMaybe[+T](value:T) extends MaybeMonad[T]{
  override def map[B](f: T ⇒ B): MaybeMonad[B] = SomeMaybe(f(value))

  override def flatMap[B](f: T ⇒ MaybeMonad[B]): MaybeMonad[B] = f(value)

  override def filter(predicate: T ⇒ Boolean): MaybeMonad[T] = if(predicate(value)) this else MaybeNot
}


object MaybeTest extends App{
  val someValue= SomeMaybe(3)
  println(someValue.map(_*2).flatMap(x ⇒ SomeMaybe(x+5)))
  println(someValue.map(_*2).flatMap(x ⇒ SomeMaybe(x+5)).filter(_%2==0))
}
