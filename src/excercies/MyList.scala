package excercies

abstract sealed class MyList[+T] {
  def head: T

  def tail: MyList[T]

  def isEmpty: Boolean

  def map[B](t: T ⇒ B): MyList[B]
  def flatMap[B](transform: T ⇒ MyList[B]): MyList[B]
  def filter(p: T ⇒ Boolean): MyList[T]

  def zipWith[S, R](otherList: MyList[S], zip: (T, S) ⇒ R): MyList[R]

  def fold[S](start: S)(operator: (S, T) ⇒ S): S

  def foreach(action: T ⇒ Unit)

  def add[S >: T](x: S): MyList[S]

  def ::[S >: T](x: S): MyList[S] = add(x)

  def ++[S >: T](x: MyList[S]): MyList[S]

  def printElements(): String

  def insertionSort(comparator: (T, T) ⇒ Int): MyList[T]

  override def toString: String = s"[${printElements()}]"
}

case object EmptyList extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: MyList[Nothing] = throw new NoSuchElementException

  override def add[S >: Nothing](x: S): MyList[S] = new Cons[S](x, EmptyList)

  override def isEmpty(): Boolean = true

  override def ++[S >: Nothing](x: MyList[S]): MyList[S] = x

  override def printElements(): String = " "

  override def map[B](t: Nothing ⇒ B): MyList[B] = EmptyList

  override def flatMap[B](transform: Nothing ⇒ MyList[B]): MyList[B] = EmptyList

  override def filter(p: Nothing ⇒ Boolean): MyList[Nothing] = EmptyList

  override def foreach(action: Nothing ⇒ Unit): Unit = ()

  override def insertionSort(
      comparator: (Nothing, Nothing) ⇒ Int): MyList[Nothing] = EmptyList

  override def zipWith[S, R](otherList: MyList[S],
                             zip: (Nothing, S) ⇒ R): MyList[R] = {
    if (otherList.isEmpty) EmptyList
    else throw new RuntimeException("Both lists must be with same size")
  }

  override def fold[S](start: S)(operator: (S, Nothing) ⇒ S): S = start
}

case class Cons[+T](h: T, t: MyList[T] = EmptyList) extends MyList[T] {

  override def add[S >: T](x: S): MyList[S] = new Cons[S](x, this)

  override def isEmpty: Boolean = false

  override def printElements(): String =
    if (tail.isEmpty)
      s"$head"
    else
      s"$head , ${tail.printElements()}"

  override def map[B](t: T ⇒ B): MyList[B] = {
    Cons(t(head), tail.map(t))
  }

  override def filter(predicate: T ⇒ Boolean): MyList[T] = {
    if (predicate(head)) Cons[T](h, tail.filter(predicate))
    else tail.filter(predicate)
  }

  override def head: T = h

  override def tail: MyList[T] = t

  override def ++[S >: T](x: MyList[S]): MyList[S] = Cons[S](h, t ++ x)

  override def flatMap[B](transform: T ⇒ MyList[B]): MyList[B] =
    transform.apply(head) ++ tail.flatMap(transform)

  override def foreach(action: T ⇒ Unit): Unit = {
    action(head)
    tail.foreach(action)
  }

  override def insertionSort(comparator: (T, T) ⇒ Int): MyList[T] = {
    //not a tailrec
    def insert(x: T, sorted: MyList[T]): MyList[T] = {
      if (sorted.isEmpty) Cons(x, EmptyList)
      else if (comparator(x, sorted.head) <= 0) Cons(x, sorted)
      else Cons(sorted.head, insert(x, sorted.tail))
    }
    val sortedTail = tail.insertionSort(comparator)
    insert(head, sortedTail)
  }

  override def zipWith[S, R](otherList: MyList[S],
                             zip: (T, S) ⇒ R): MyList[R] = {
    if (otherList.isEmpty)
      throw new RuntimeException("Both lists must have the same size")
    else Cons(zip(head, otherList.head), tail.zipWith(otherList.tail, zip))
  }

  override def fold[S](start: S)(operator: (S, T) ⇒ S): S = {
    val newStart = operator(start, head)
    tail.fold(newStart)(operator)
  }

}

object TestList extends App {

  val stringList: MyList[String] = EmptyList
  val emptyIntList: MyList[Int] = EmptyList
  val intList1 = new Cons[Int](1, new Cons[Int](2, new Cons[Int](3)))
  val intList2 = 4 :: 5 :: EmptyList
  println(intList1.add(0).head)
  println(intList2.tail.head)
  //println(list.tail.tail.tail.head) // throws NoSuchElementException
  val combined = intList1 ++ intList2
  val nameList = "aviad" :: "shahar" :: "alon" :: EmptyList
  println(nameList)
  println(combined)
  println(combined.map(x ⇒ x * 2))
  //creates a list of (n,n+1) for each element
  println(combined.flatMap(x ⇒ x :: x + 1 :: EmptyList))

  intList1.foreach(println)
  val unsorted: MyList[Int] = intList2 ++ intList1
  println(unsorted)
  println(unsorted.insertionSort((x, y) ⇒ x - y))
  println(nameList.zipWith[Int, String](intList1, _ + ":" + _))
  println(intList1.fold(0)(_ + _))

  println("supports for-comprehension (map,flatMap,filter are exists) the exact signature is important")
  println(
            for {
            a← intList1
            b← intList2
          } yield a+" "+b
  )
}
