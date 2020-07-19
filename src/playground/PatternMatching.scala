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

}
