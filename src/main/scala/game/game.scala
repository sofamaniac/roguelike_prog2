package game

import entity._
import map._
import position._
import graphics._
import scalafx.scene.input.KeyCode

object Game
{
    val player = new Player(GameWindow.contextGame)
    var currentPhase = "move"
    var currentActor:ControlledEntity= player
    val cursor = new Cursor(GameWindow.contextGame)

    def eventHandler(kc:KeyCode)
    {
      kc.getName match
      {
        case "Right"  => currentActor.rotate(1)
        case "Left"   => currentActor.rotate(-1)
        case "Up"     => currentActor.move(currentActor.getDir(1))
        case "Down"   => currentActor.move(currentActor.getDir(-1))
        case "A"      => cursor.pos.x = player.pos.x
                         cursor.pos.y = player.pos.y
                         currentActor = if(currentPhase == "attack") player else cursor
                         currentPhase = if(currentPhase == "attack") "move"  else "attack"
        case _        => ()
      }
    }

    def initialization()
    {
        // generate map :
        val origin:Tile = new Tile(new Point(0,0))
        // create Player
        // create and place enemies
        // create and place items
    }

    def loop()
    {
      while (player.hp > 0)
        {
            // get action(s) from player
            // resolve actions (dodge from ennemies)
            // ennemies turn
            // dodge for the player
        }
    }
}
