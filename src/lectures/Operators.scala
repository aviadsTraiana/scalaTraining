package lectures

object Operators extends App {

  val p=new Person("Shahar",25)
  println(new Person("aviad",30).age)
  println(new Person("aviad",30).birthday.age)
  println((p birthday) ! "happy birthday")
}


class Person(val name: String, val age:Int = 20){
  def birthday = new Person(name,age+1)
  def !(s:String) = s"Hey my name is $name , my age is $age and this is what I have to say: $s!!"
}