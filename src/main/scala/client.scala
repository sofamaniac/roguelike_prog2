package client

import socket._
import java.net.{ ServerSocket, SocketException, SocketTimeoutException, InetAddress, Socket => JSocket }
import java.io.IOException
import map.Map

object Client {
  val PORT = 6666
  val TIMEOUT = 1000

  val socket = new Socket(new JSocket("localhost", PORT))

  val sep = Request.sep
  val end = Request.end

  val in  = socket.inputStream()

  def init():Unit=
  {
  }

  def read():String =
  {
    var b:Int = -2
    var res:Array[Byte] = Array[Byte]()
    val endcode = end.getBytes
    var counter = 0
    while ((b != -1 || res.length != 0) && counter != endcode.length)
    {
      b = in.read()
      res = res :+ b.toByte
      if (b == endcode(counter))
        counter += 1
      else
        counter  = 0
    }
    val s = Gzip.decompress(res)
    s match
    {
      case Some(str) => {println(StringContext treatEscapes (str));return StringContext treatEscapes (str)}
      case _ => return ""
    }
  }

  def getMap():Map=
  {
    val out = socket.outputStream()
    out.write(("REQUEST"+sep+"MAP"+sep+end).getBytes())
    return upickle.default.read[Map](get_answer(TIMEOUT).substring(1).dropRight(1))
  }
  
  def get_answer(timeout:Int):String=
  {
    val time_start = System.currentTimeMillis()
    var res:String = ""
    while(System.currentTimeMillis() - time_start <= timeout && res == "")
    {
      res = read()
    }
    if (res == "")
    {
      throw new Exception
    }
    return res
  }
}
