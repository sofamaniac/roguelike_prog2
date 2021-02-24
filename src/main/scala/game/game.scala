package game

import entity.AC_enemy

class Tile(val coord:Point)
{
    var adjArr:List[Tile] = List()

    var item:Option[Item] = None
    var entity:Option[SentientEntity] = None

    val texture:Option[GraphicEntity] = None        // TODO : to change
}

object Game(val player:Player)
{
    val player = new Player()

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
        while (player.hp)
        {
            // get action(s) from player
            // resolve actions (dodge from ennemies)
            // ennemies turn
            // dodge for the player
        }
    }
}