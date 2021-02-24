package game

import entity._
import map._
import position._
import graphics._
import scalafx.scene.input.KeyCode

object Game
{
    val player = new Player(GameWindow.contextGame)

    def eventHandler(kc:KeyCode)
    {

      kc.getName match
      {
        case "Right" => player.rotate(1)
        case "Left"  => player.rotate(-1)
        case "Up"    => player.move(player.getDir(1))
        case "Down"  => player.move(player.getDir(-1))
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
