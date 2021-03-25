package enemy

import entity._
import item._
import position._
import scalafx.scene.canvas._
import scalafx.scene.image._
import graphics._
import game._
import map._

import upickle.default._
object Enemy
{
  // we define how are object serialized, 
  // for instance here, only the name and the maxHP would be save to Json
  // whereas, we create a new Enemy using only the first field of the json as a name
  implicit val rw: ReadWriter[Enemy] = 
    readwriter[ujson.Value].bimap[Enemy](
      e=> ujson.Arr(e.name, e.maxHP),
      json => new Enemy(new Point(5,3), name=json("name").str, weapon=new Weapon("Cone Weapon example", 1000000, 5, "pow", Zones.cone, 1, 0, 8, 5, 8))
      )
}

class Enemy(override val pos:Point, val name:String, var maxHP:Int=100, var armorClass:Int=30, var baseAP:Int=5, var modifAP:Int=0, var baseStr:Int=10, var modifStr:Int=0, var baseDex:Int=10, var modifDex:Int=0, var basePow:Int=10, var modifPow:Int=0, var weapon:Weapon)
    extends SentientEntity(AnimationLoader.load("goblin.png", 11, sizeY=58), pos)
{
  var curHP = maxHP
  var curAP = baseAP

  def IA():Unit =
  {
    val next = findBestMove()
    move(next)
    var i = 0
    for(i <- 0 until dirArray.size)
    {
      if(curAP > 0 && weapon.zone(weapon, i, pos, Game.player.pos))
      {
        // the fun consequence of this way of attacking is that enemies can damage other enemies
        weapon.attack(Game.player.pos, this, i)
        curAP = 0
      }
    }
  }

  def findBestMove():Point =
  {
    var i = 0
    var j = 0
    // if player is in range, does not move
    for(i <- 0 until dirArray.size)
    {
      if(weapon.zone(weapon, i, pos, Game.player.pos))
      {
        return pos
      }
    }

    // else find a position that move it closer to the player 
    // in the future, enemies will have like the player a detection range,
    // outside of which they are unable to see the player
    val curD = pos.distance(Game.player.pos)
    for (i <- 0 until Map.tileArray.size)
    {
      for (j <- 0 until Map.tileArray(i).size)
      {
        if(Map.tileArray(i)(j).coord.distance(Game.player.pos) < curD
            && Map.tileArray(i)(j).coord.distance(pos) < curAP)
          return Map.tileArray(i)(j).coord
      }
    }
    return pos
  }
  def dodge():Boolean = {return false}
  def loot():Unit = 
  {
    val item = new Key
    item.pos.setPoint(pos)
    Map.fromPoint(pos).item = Some(item)
  }
}

// Définir les caractéristiques des ennemis (caractéristiques, path vers les animations, ...) dans un format
// de fichier type Json, cependant json n'est par défaut pas supporté par Scala
