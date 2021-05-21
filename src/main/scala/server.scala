package server

import socket._
import java.net.{ ServerSocket, SocketException, SocketTimeoutException, InetAddress, Socket => JSocket }
import java.io.IOException
import upickle._
import ujson._

import map.Map


object Server {
  val PORT = 6666

  def start():Unit = {
    val server_socket = new ServerSocket(PORT)

    var socket:JSocket = null

    while(true)
    {
      try
      {
        socket = server_socket.accept()
      }
      catch
      {
        case e:IOException => println("I/O Error : " + e)
      }
      // start new thread for client
      println("A new client is connected")
      new ClientThread(new Socket(socket)).start()
    }
  }
}

class ClientThread(val socket:Socket) extends Thread
{
  val in  = socket.inputStream()
  val out = socket.outputStream()
  val sep = Request.sep
  val end = Request.end
  override def run():Unit=
  {
    while(true)
    {
      val s = read()
      s.length() match
      {
        case 0 => ()
        case _ => parse(s)
      }
    }
  }
  def read():String =
  {
    var b:Int = -2
    var res:String = ""
    val endcode = end.getBytes
    var counter = 0
    while ((b != -1 || res != "") && counter != endcode.length)
    {
      b = in.read()
      res = (res + (b.toChar).toString)
      if (b == endcode(counter))
        counter += 1
      else
        counter  = 0
    }
    return res
  }

  def parse(s:String):Unit=
  {
    val s_arr = s.split(sep)
    if (s_arr.length < 2)
    {
      return
    }
    s_arr(0) match
    {
      case "REQUEST" => handle_request(s_arr(1))
      case "COMMAND" => handle_command(s_arr(2))
      case _ => return // TODO: raise exception
    }
  }

  def handle_request(s:String):Unit=
  {
    s match
    {
      case "MAP" => {val s1 = Gzip.compress((upickle.default.write(Map.map)).getBytes)++(end.getBytes);println(s1.length); out.write(s1)}
      case _ => ()
    }
  }

  def handle_command(s:String):Unit=
  {
    println("got a command")
  }

}
