import graphics._
import game._
import scalafx.application.JFXApp

import server._
import client._


object Program extends JFXApp
{
    System.setProperty("quantum.multithreaded", "false");

    println("Type 0 to create server, 1 to create client.")

    val what = readInt()
    if(what == 0)
    {
      println("Starting server.")
      GameServer.initialization()
      Server.start()
      println("Server started.")
    }

    else
    {
      println("Starting client.")
      Client.init()
      GameWindow.start()
      println("Client started.")
    }
}
