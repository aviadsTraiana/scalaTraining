package excercies

trait MyTransformer[-A,B] {
  def transform(a: A):B
}