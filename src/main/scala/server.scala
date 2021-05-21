package server

import socket._
import java.net.{ ServerSocket, SocketException, SocketTimeoutException, InetAddress, Socket => JSocket }
import java.io.IOException
import upickle._
import ujson._

import map.Map
import game._
import item._
import weapon._
import position._
import entity._

import json._


object Server {
  val PORT = 6666

  var connectedClients:Vector[ClientThread] = Vector[ClientThread]()

  var wasStarted = false

  def start():Unit = {
    val server_socket = new ServerSocket(PORT)
    
    wasStarted = true

    var socket:JSocket = null

    while(true)
    {
      Thread.sleep(15)
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
      val client = new ClientThread(new Socket(socket))
      client.start()
      connectedClients = connectedClients :+ client
    }
  }

  def handle_command(id:String, command:String):Unit=
  {
    GameServer.player = GameServer.playerVect(id)
    if (GameServer.hasEndedTurn(id))
      return
    val params = command.split("/")
    params(0) match
    {
      case "ATTACK" => handle_attack(params)
      case "INVENTORY" => handle_inventory(params)
      case "MOVE" => handle_move(params)
      case "SPEAK" => handle_speak(params)
      case "END_TURN" => GameServer.hasEndedTurn(id) = true
      case _ => println(command)
    }
  }

  def handle_attack(params:Array[String]):Unit=
  {
    val pos = params(1).split(",")
    val dest = new Point(pos(0).toInt, pos(1).toInt)
    val dir = pos(2).toInt
    val weapon = WeaponCreator.create(params(2))

    GameServer.currentWeapon = weapon

    GameServer.cursor.pos.setPoint(dest)
    GameServer.cursor.currentDir = dir
    GameServer.player.attack(dest)
  }

  def handle_move(params:Array[String]):Unit=
  {
    val dest = new Point(params(1).split(",")(0).toInt, params(1).split(",")(1).toInt)
    GameServer.player.move(dest)
  }

  def handle_inventory(params:Array[String]):Unit=
  {
    val action = params(1)

    action match
    {
      case "ADD" => { GameServer.player.inventory.add(ItemCreator.create(params(2)))}
      case "DROP" => {GameServer.player.inventory.curInv = params(2).toInt; GameServer.player.inventory.drop()}
      case "PICKUP" => GameServer.pickUp()
      case "SELL" => GameServer.sell()
      case "USE" =>  GameServer.player.inventory.useItem()
    }
  }

  def handle_speak(params:Array[String]):Unit=
  {
  }

  def sendAction(action:String):Unit=
  {
    if(!wasStarted)
      return
    connectedClients.foreach( c => c.actionToSend = c.actionToSend :+ action )
  }
}

class ClientThread(val socket:Socket) extends Thread
{
  val in  = socket.inputStream()
  val out = socket.outputStream()
  val sep = Request.sep
  val end = Request.end

  var actionToSend:Vector[String] = Vector[String]("NONE")

  val id = socket.jsocket.getInetAddress().getHostAddress().toString

  GameServer.createPlayer(id)

  override def run():Unit=
  {
    Request.send(Array("ID", id), out)
    while(true)
    {
      GameServer.loop()
      val s = Request.receive(in)
      s.length() match
      {
        case 0 => ()
        case _ => parse(s)
      }
      Thread.sleep(15)
    }
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
      case "COMMAND" => handle_command(s_arr(1))
      case _ => return // TODO: raise exception
    }
  }

  def handle_request(s:String):Unit=
  {
    s match
    {
      case "MAP" => {Request.send(Array(upickle.default.write(Map.map)), out)}
      case "UPDATE" => {Request.send(actionToSend.toArray, out); actionToSend = Vector("NONE")}
      case _ => ()
    }
  }

  def handle_command(s:String):Unit=
  {
    Server.handle_command(id, s)
  }

}
