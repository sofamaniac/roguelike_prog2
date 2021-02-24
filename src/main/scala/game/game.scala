package game

import entity._
import map._
import position._
import graphics._

object Game
{
    val player = new Player(GameWindow.contextGame)

    def eventHandler()
    {

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
