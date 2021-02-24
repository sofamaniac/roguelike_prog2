package map

import item._
import graphics._
import entity._
import position._

class Tile(val coord:Point)
{
    var isWalkable:Boolean = false

    var item:Option[Item] = None
    var entity:Option[SentientEntity] = None

    val texture:Option[GraphicEntity] = None        // TODO : to change
}

