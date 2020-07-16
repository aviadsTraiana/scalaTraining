package playground
import scala.reflect.runtime.universe._
object TypeErasure extends App{
  //this will not work:
  def determineListType[T](list: List[T]): Unit = list match {
    case intList: List[Int] => println(s"List of integers: $intList")
    case strList: List[String] => println(s"List of Strings: $strList")
  }
  determineListType(List[Int](1,2,3))
  determineListType(List[String]("1","2"))

  //this will work
  // can be determined with typeTag
  println(typeTag[List[Int]].tpe)
}
