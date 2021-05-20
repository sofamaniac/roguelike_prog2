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
    var b = in.read()
    var res:Array[Byte] = new Array(0)
    val endcode = end.getBytes
    while (b.toInt != -1 && b != endcode(0))
    {
      res = (new String(res) + (b.toChar).toString).getBytes()
      b = in.read()
    }
    return new String(res)
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
    val s = Gzip.compress((upickle.default.write(Map.map)+end).getBytes)
    out.write(s)
  }

  def handle_command(s:String):Unit=
  {
    println("got a command")
  }

}
