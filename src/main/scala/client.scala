package client

import socket._
import java.net.{ ServerSocket, SocketException, SocketTimeoutException, InetAddress, Socket => JSocket }
import java.io.{ BufferedOutputStream, BufferedReader, InputStream, OutputStream }
import java.io.IOException
import map.Map
import item._
import weapon._
import position._
import game._

object Client {
  val PORT = 6666
  val TIMEOUT = 1000

  var socket:Socket = null
  val sep = Request.sep

  var in:InputStream = null
  var out:OutputStream = null

  var id = ""

  def init():Unit=
  {
    socket =  new Socket(new JSocket("localhost", PORT))
    in = socket.inputStream()
    out = socket.outputStream()
    id = get_answer(TIMEOUT).split(sep)(1) 
  }

  def sendAction(action:String):Unit=
  {
    Request.send(Array("COMMAND", action), out)
  }


  def getMap():Map=
  {
    Request.send(Array("REQUEST", "MAP"), out)
    return upickle.default.read[Map](get_answer(TIMEOUT).substring(1).dropRight(1))
  }

  def getUpdate():Unit=
  {
    Request.send(Array("REQUEST", "UPDATE") ,out)
    val todo = get_answer(TIMEOUT).split(sep)
    todo.foreach( a => handle_action(a))
  }
  
  def get_answer(timeout:Int):String=
  {
    val time_start = System.currentTimeMillis()
    var res:String = ""
    while(System.currentTimeMillis() - time_start <= timeout && res == "")
    {
      res = Request.receive(in)
    }
    if (res == "")
    {
      throw new Exception
    }
    return res
  }

  def handle_action(a:String):Unit=
  {
    val params = a.split("/")
    params(0) match
    {
      case "MOVE" => handle_move(params)
      case "ATTACK" => handle_attack(params)
      case "DAMAGE" => handle_damage(params)
      case "INVENTORY" => handle_inventory(params)
      case "END_TURN" => GameClient.loop()
      case _ => ()
    }
  }

  def handle_move(params:Array[String]):Unit=
  {
    val start = new Point(params(1).split(",")(0).toInt, params(1).split(",")(1).toInt)
    val dest = new Point(params(2).split(",")(0).toInt, params(2).split(",")(1).toInt)

    val tile = Map.fromPoint(start)

    tile.entity match
    {
      case Some(e) => e.move(dest)
      case _ => ()
    }
  }

  def handle_attack(params:Array[String]):Unit=
  {
  }
  def handle_inventory(params:Array[String]):Unit=
  {
  }
  def handle_damage(params:Array[String]):Unit=
  {
    val start = new Point(params(1).split(",")(0).toInt, params(1).split(",")(1).toInt)
    val dam = params(2).toInt

    val tile = Map.fromPoint(start)

    tile.entity match
    {
      case Some(e) => e.damage(dam, GameClient.player)
      case _ => ()
    }

  }
}
