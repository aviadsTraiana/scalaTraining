package playground

object Combinations extends App{

  val intList = List(1,2,3)
  val stringList= List("a","b","c","d")
  val colorList= List("red","blue")
  def combinations[A,B](a:List[A],b:List[B]):List[String] = {
    a.flatMap(x⇒ b.map(y⇒ s"$x:$y"))
  }
  println(combinations(intList,stringList))

  //can be replaced with for-comprehensions
  val forCombination = for {
    a← intList
    b← stringList
    c← colorList
  } yield s"$a:$b:$c"
  println(forCombination)
  //with 'filter'
  val forCombination2 = for {
    a← intList if a % 2 ==0
    b← stringList
    c← colorList
  } yield s"$a:$b:$c"
  println(forCombination)

  for {
    n← intList
  } println(n)
}
