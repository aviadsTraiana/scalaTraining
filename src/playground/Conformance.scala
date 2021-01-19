package playground

object Conformance {
  trait S
  trait SS extends S
  trait G
  trait R {
    type ST <: S
    type GT <: G

    def getS: ST
    def getG: GT
  }
  trait RT[SD <: S, GD <: G] extends R {
    override type ST = SD
    override type GT = GD
  }

  final case class ConcreteS1() extends SS
  final case class ConcreteS2() extends SS
  final case class ConcreteS3() extends SS

  final case class ConcreteG1() extends G
  final case class ConcreteG2() extends G
  final case class ConcreteG3() extends G

  final class C1 extends RT[ConcreteS1, ConcreteG1] {
    override def getS: ConcreteS1 = ConcreteS1()

    override def getG: ConcreteG1 = ConcreteG1()
  }
  //some different impl
  final class C2 extends RT[ConcreteS2, ConcreteG2]{
    override def getS: ConcreteS2 = ConcreteS2()

    override def getG: ConcreteG2 = ConcreteG2()
  }
  //some different impl
  final class C3 extends RT[ConcreteS3, ConcreteG3]{
    override def getS: ConcreteS3 = ConcreteS3()

    override def getG: ConcreteG3 = ConcreteG3()
  }

  //generic impl over W
  trait GeneralizationOverW[ST <: S,GT <: G , CON <: RT[ST, GT]] {
    //  With `<: RT[S, G]` restriction I know C is RT with some S and G I can use S and G behaviours
  }

  class W1 extends GeneralizationOverW[ConcreteS1,ConcreteG1,C1]
  class W2 extends GeneralizationOverW[ConcreteS2,ConcreteG2,C2]
  class W3 extends GeneralizationOverW[ConcreteS3,ConcreteG3,C3]


}
