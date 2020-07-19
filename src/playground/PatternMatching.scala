package playground

object PatternMatching extends App{

  abstract class Expr
  case class Number(n:Int) extends Expr
  case class Sum(expr1: Expr,expr2: Expr) extends Expr
  case class Product(expr1: Expr,expr2: Expr) extends Expr

  def evaluateExpression(expr: Expr):Int= {
    expr match {
      case Number(x:Int) ⇒ x
      case Sum(expr1,expr2) ⇒ evaluateExpression(expr1)+ evaluateExpression(expr2)
      case Product(expr1,expr2) ⇒ evaluateExpression(expr1) * evaluateExpression(expr2)
    }
  }

  println(evaluateExpression(Sum(Product(Number(2),Number(3)),Number(5))))

  val tuple= ("one","two")
  val (a,b) = tuple
  println(a+" and "+b)

  val list= List(1,2,3,4)
  val head :: tail = list
  println(head)
  println(tail)

  //partial function literal
  val mappedList=list.map{
    case 1 ⇒ "one"
    case 2 ⇒ "two"
    case 3⇒ "three"
    case _ ⇒ "other"
  }
  println(mappedList) //List(one, two, three, other)

}
