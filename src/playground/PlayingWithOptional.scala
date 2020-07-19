package playground

import scala.util.Random

object PlayingWithOptional extends App{
  val map: Map[String,String] = Map(
    "host" → "127.0.0.1",
    "port" → "80"
  )
  class Connection{
    def connect ="connected"
  }
  object Connection{
    val random=new Random(System.nanoTime())
    def apply(host:String,port:String) = if( random.nextBoolean()) Some(new Connection) else None
  }

  val host= map.get("host")
  val port = map.get("port")

  val connection: Option[Connection] = host.flatMap(h⇒ port.flatMap(p⇒Connection(h,p)))
  val connectionStatus: Option[String] = connection.map(c ⇒ c.connect)
  println(connectionStatus)
  connection.foreach(println)

  //in for-comprehension
  val forConnectionStatus= for {
    h ← map.get("host")
    p ← map.get("port")
    c ← Connection(host=h,port=p)
  } yield c.connect
  forConnectionStatus.foreach(println)

}
