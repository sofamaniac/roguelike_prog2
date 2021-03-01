package enemy

import entity._
import item._
import position._
import scalafx.scene.canvas._
import scalafx.scene.image._
import graphics._
import game._
import map._

class Enemy(pos:Point, dest:GraphicsContext, val name:String, var maxHP:Int, var curHP:Int, var armorClass:Int, var baseAP:Int, var modifAP:Int, var curAP:Int, var baseStr:Int, var modifStr:Int, var baseDex:Int, var modifDex:Int, var basePow:Int, var modifPow:Int, var weapon:Weapon)
    extends SentientEntity(AnimationLoader.load("goblin.png", 11, sizeY=58), pos, dest)
{
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
  def loot():Unit = {}
}

// Définir les caractéristiques des ennemis (caractéristiques, path vers les animations, ...) dans un format
// de fichier type Json, cependant json n'est par défaut pas supporté par Scala
