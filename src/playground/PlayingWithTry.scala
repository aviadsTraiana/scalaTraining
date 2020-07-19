package playground

import scala.util.{Random, Try}

object PlayingWithTry extends App{

  val host="localhost"
  val port ="8080"
  def renderHtml(page:String) =println(page)

  class Connection{
    def get(url:String):String ={
      val random= new Random(System.nanoTime())
      if( random.nextBoolean()) "<html>...</html>"
      else throw new RuntimeException("can't fetch content")
    }
  }

  object HttpService{
    val random= new Random(System.nanoTime())
    def getConnection(host:String,port:String) :Connection = if(random.nextBoolean()) new Connection
    else throw new RuntimeException("Port is already in use")
  }


  Try(HttpService.getConnection(host,port))
    .flatMap(connection ⇒ Try(connection.get(s"$host:$port")))
      .map(page ⇒ renderHtml(page))

  println("for-comprehension")
  for {
    connection ← Try(HttpService.getConnection(host, port))
    page ← Try(connection.get(s"$host:$port"))
  } yield renderHtml(page)
}
