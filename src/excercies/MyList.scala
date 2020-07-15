package excercies


abstract sealed class MyList[+T] {
  def head: T

  def tail: MyList[T]

  def isEmpty(): Boolean

  def map[B](t: MyTransformer[T, B]): MyList[B]

  def flatmap[B](t: MyTransformer[T, MyList[B]]): MyList[B]

  def filter(p: MyPredicate[T]): MyList[T]

  def add[S >: T](x: S): MyList[S]

  def ::[S >: T](x: S): MyList[S] = add(x)

  def ++[S >: T](x: MyList[S]): MyList[S]

  def printElements(): String

  override def toString: String = s"[${printElements()}]"
}

case object EmptyList extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: MyList[Nothing] = throw new NoSuchElementException

  override def add[S >: Nothing](x: S): MyList[S] = new Cons[S](x, EmptyList)

  override def isEmpty(): Boolean = true

  override def ++[S >: Nothing](x: MyList[S]): MyList[S] = x


  override def printElements(): String = " "

  override def map[B](t: MyTransformer[Nothing, B]): MyList[B] = EmptyList

  override def flatmap[B](t: MyTransformer[Nothing, MyList[B]]): MyList[B] = EmptyList

  override def filter(p: MyPredicate[Nothing]): MyList[Nothing] = EmptyList
}

case class Cons[+T](h: T, t: MyList[T] = EmptyList) extends MyList[T] {
  override def add[S >: T](x: S): MyList[S] = new Cons[S](x, this)

  override def isEmpty(): Boolean = false

  override def printElements(): String =
    if (tail.isEmpty())
      s"$head"
    else
      s"$head , ${tail.printElements()}"

  override def head: T = h

  override def tail: MyList[T] = t

  override def map[B](t: MyTransformer[T, B]): MyList[B] = {
    Cons(t.transform(head), tail.map(t))
  }


  override def filter(p: MyPredicate[T]): MyList[T] = {
    if (p.test(head)) Cons[T](h, tail.filter(p)) else tail.filter(p)
  }

  override def ++[S >: T](x: MyList[S]): MyList[S] = Cons[S](h, t ++ x)
  override def flatmap[B](t: MyTransformer[T, MyList[B]]): MyList[B] = t.transform(head) ++ tail.flatmap(t)
}

object TestList extends App {

  val stringList: MyList[String] = EmptyList
  val emptyIntList: MyList[Int] = EmptyList
  val intList1 = new Cons[Int](1, new Cons[Int](2, new Cons[Int](3)))
  val intList2= 4 :: 5 :: EmptyList
  println(intList1.add(0).head)
  println(intList2.tail.head)
  //println(list.tail.tail.tail.head) // throws NoSuchElementException
  val combined = intList1 ++ intList2
  println("aviad" :: "shahar" :: EmptyList)
  println(combined)
  println(combined.map(x ⇒ x*2 ))
  //creates a list of (n,n+1) for each element
  println(combined.flatmap( x ⇒ x:: x+1 :: EmptyList))




}