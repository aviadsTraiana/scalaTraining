package excercies

trait MyPredicate[-T] {
  def test(x:T) : Boolean
}