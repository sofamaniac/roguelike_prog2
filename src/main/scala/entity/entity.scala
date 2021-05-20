package entity

import graphics._
import messageHandler._
import animation._
import scalafx.Includes._
import scalafx.scene.image._
import scalafx.scene.canvas._
import position._
import map._
import game._
import item._
import weapon._
import json._
import upickle.default._
import enemy._

abstract class Entity(animation:Animation, pos:Point, dest:GraphicsContext) 
    extends GraphicEntity(animation, pos, dest)
{
    val name:String
    def move(dir:Point):Unit
}

import scala.reflect.ClassTag
import scala.reflect._
object SentientEntity
{
  implicit val rw: ReadWriter[SentientEntity]=
    readwriter[ujson.Value].bimap[SentientEntity](
      e => write(e),
      json => upickle.default.read[SentientEntity](json)
    )

  def write(e:SentientEntity):ujson.Value=
  {
    if (e.isInstanceOf[Merchant])
      return upickle.default.write(e.asInstanceOf[Merchant])
    else if (e.isInstanceOf[NeutralNPC])
      return upickle.default.write(e.asInstanceOf[NeutralNPC])
    else if (e.isInstanceOf[CowardNPC])
      return upickle.default.write(e.asInstanceOf[CowardNPC])
    else if (e.isInstanceOf[Player])
      return upickle.default.write(e.asInstanceOf[Player])


    var res = "{"
        res += s"name:${upickle.default.write(e.name)},"
        res += s"currentRoomCoords:${upickle.default.write(e.currentRoomCoords)},"
        res += "lastHitBy:\"this\","
        res += s"weapon:${upickle.default.write(e.weapon)},"
        res += s"maxHP:${upickle.default.write(e.maxHP)},"
        res += s"curHP:${upickle.default.write(e.curHP)},"
        res += s"curWeight:${upickle.default.write(e.curWeight)},"
        res += s"maxWeight:${upickle.default.write(e.maxWeight)},"
        res += s"gold:${upickle.default.write(e.gold)},"
        res += s"armorClass:${upickle.default.write(e.armorClass)},"
        res += s"baseAP:${upickle.default.write(e.baseAP)},"
        res += s"modifAP:${upickle.default.write(e.modifAP)},"
        res += s"curAP:${upickle.default.write(e.curAP)},"
        res += s"baseStr:${upickle.default.write(e.baseStr)},"
        res += s"modifStr:${upickle.default.write(e.modifStr)},"
        res += s"baseDex:${upickle.default.write(e.baseDex)},"
        res += s"modifDex:${upickle.default.write(e.modifDex)},"
        res += s"basePow:${upickle.default.write(e.basePow)},"
        res += s"modifPow:${upickle.default.write(e.modifPow)},"
        res += s"poisonDuration:${upickle.default.write(e.poisonDuration)},"
        res += s"poisonDamage:${upickle.default.write(e.poisonDamage)},"
        res += s"onFireDuration:${upickle.default.write(e.onFireDuration)},"
        res += s"onFireDamage:${upickle.default.write(e.onFireDamage)},"
        res += s"frozenDuration:${upickle.default.write(e.frozenDuration)},"
        res += s"frozenDamage:${upickle.default.write(e.frozenDamage)},"
        res += s"paralyzedDuration:${upickle.default.write(e.paralyzedDuration)},"
        res += s"paralyzedDamage:${upickle.default.write(e.paralyzedDamage)},"
        res += s"regenDuration:${upickle.default.write(e.regenDuration)},"
        res += s"regenHP:${upickle.default.write(e.regenHP)},"
        /*
        res += s"inventory:${upickle.default.write(e.inventory)},"
        res += s"helmet:${upickle.default.write(e.helmet)},"
        res += s"chestplate:${upickle.default.write(e.chestplate)},"
        res += s"leggings:${upickle.default.write(e.leggings)},"
        res += s"boots:${upickle.default.write(e.boots)},"
        */

    /*
    val intAttr = List("curHP", "curWeight", "maxWeight",  "gold", "armorClass", "baseAP", "modifAP", "curAP",
      "baseStr", "modifStr", "baseDex", "modifDex", "basePow", "modifPow", "poisonDuration", "poisonDamage",
      "onFireDuration", "onFireDamage", "frozenDuration", "frozenDamage", "paralyzedDuration", "paralyzedDamage",
      "regenDuration", "regenHP")

    intAttr.foreach( n =>
        {
        val f = classTag[SentientEntity].runtimeClass.getDeclaredField(n)
        f.setAccessible(true)
        res += s"$n:${upickle.default.write(f.get(e).asInstanceOf[Int])},"
        f.setAccessible(false)
        }
      )
    */
    return res + "}"
  }
}

abstract class SentientEntity(animation:Animation, pos:Point) 
    extends Entity(animation, pos, GameWindow.contextGame)
{
    val name:String
    var maxHP:Int           // health points
    var curHP:Int           // current hp
    var currentRoomCoords = new Point(0, 0) // keep the coordinate of the current room in which the entity is

    var curWeight = 0
    var maxWeight = 25

    var gold = 0
    var lastHitBy:SentientEntity = this // We store the last entity than inflicted damage
    var armorClass:Int      // AC
    
    var baseAP:Int          // Action Points
    var modifAP:Int
    var curAP:Int
    
    var baseStr:Int         // starting value
    var modifStr:Int        // relative modifications
    
    var baseDex:Int 
    var modifDex:Int

    var basePow:Int 
    var modifPow:Int

    var weapon:Weapon       // equipped weapon
    var inventory:Inventory = new Inventory(this, MessageHandler.tradeZone)

    var poisonDuration:Int  = 0 // number of turn before end of poison effect
    var poisonDamage:Int    = 0

    var onFireDuration:Int  = 0
    var onFireDamage:Int    = 0

    // While frozen the entity can still move, but gets less AP and takes damage
    var frozenDuration:Int  = 0
    var frozenDamage:Int    = 0

    // While paralyzed the entity can only use item but takes no damage
    var paralyzedDuration:Int = 0
    var paralyzedDamage:Int   = 0

    var regenDuration:Int   = 0
    var regenHP:Int         = 0

    // We setup the armor
    var helmet:Helmet = ItemCreator.create("chainmail helmet").asInstanceOf[Helmet]
    var chestplate:Chestplate = ItemCreator.create("chainmail chestplate").asInstanceOf[Chestplate]
    var leggings:Leggings = ItemCreator.create("chainmail leggings").asInstanceOf[Leggings]
    var boots:Boots = ItemCreator.create("chainmail boots").asInstanceOf[Boots]

    // And the jewelry
    // var ring1:Item = new Jewel
    // var ring2:Item = new Jewel
    // var Pendant:Item = new Jewel
    
    val dirArray = Array(new Point(1, 0), new Point(0, 1), new Point(-1, 1), new Point(-1, 0), new Point(0, -1), new Point(1, -1))

    def move(next:Point):Unit = 
    {
      if (isMoveValid(next))
      {
        Map.fromPoint(pos).entity = None
        Map.fromPoint(next).entity = Some(this)
        curAP -= pos.distance(next)
        pos.setPoint(next)
      }
    }

    def isMoveValid(next:Point):Boolean =
    {
      return (Map.isInbound(next) && Map.fromPoint(next).entity == None && Map.fromPoint(next).walkable && pos.distance(next) <= curAP
              && Map.inSight(pos, next))
    }


    def getInfo():String =
    {
      return "%s : %s/%s HP".format(name, curHP, maxHP)
    }

    def damage(dam:Int, from:SentientEntity):Unit=
    {
      // [from] can be used to apply a thorn-like effect
      lastHitBy = from
      curHP -= dam
      if(curHP <= 0)
      {
        kill()
      }
    }

    def heal(hp:Int):Unit = 
    {
      curHP = maxHP.min(curHP + hp)
    }

    def attack(dest:Point, dir:Int):Unit =
    {
      if(curAP > 0) {
        curAP = 0
        weapon.attack(dest, this, dir)
      }
    }

    def addEffects(fireDur:Int=0, fireDam:Int=0, poisDur:Int=0, poisDam:Int=0, frozDur:Int=0, frozDam:Int=0, paraDur:Int=0, paraDam:Int=0)
    {
      // TODO: change so that the effect are not simply added
      onFireDuration  = onFireDuration + fireDur
      onFireDamage    = onFireDamage + fireDam

      poisonDuration  = poisonDuration + poisDur
      poisonDamage    = poisonDamage + poisDam

      frozenDuration  = frozenDuration + frozDur
      frozenDamage    = frozenDamage + frozDam

      paralyzedDuration = paralyzedDuration + paraDur
      paralyzedDamage   = paralyzedDamage + paraDam
    }

    def applyEffects():Unit={
      if (poisonDuration > 0){
        damage(poisonDamage, lastHitBy)
        poisonDuration -= 1
      }
      if (onFireDuration > 0){
        damage(onFireDamage, lastHitBy)
        onFireDuration -= 1
      }
      if (frozenDuration > 0){
        damage(frozenDamage, lastHitBy)
        frozenDuration -= 1
      }
      if (paralyzedDuration > 0){
        damage(paralyzedDamage, lastHitBy)
        paralyzedDuration -= 1
      }
      if (regenDuration > 0){
        heal(regenHP)
        regenDuration -= 1
      }
    }

    def applyMagicCost(cost:Int):Unit =
    {
      // This function can be changed to add resistance to the costs of magic use
      curHP -= cost
      if(curHP <= 0)
      {
        kill()
      }
    }

    def kill():Unit =
    {
      lastHitBy.gold += 10  // TODO change to drop a random value
      Map.fromPoint(pos).entity=None
      loot() // TODO: change to not override it there is already an item
    }

    def endTurn():Unit =
    {
      applyEffects()
      if (paralyzedDuration > 0)
        curAP = 0
      if (frozenDuration > 0)
        curAP = (baseAP + modifAP)/ 2
      else
        curAP = baseAP + modifAP
    }

    def dodge():Boolean

    def loot() // Generate loot on death
}


class Cursor(dest:GraphicsContext)
  extends Entity(new Animation("cursor.png", 1), new Point(0,0), dest)
{
  val arrow = new GraphicEntity(new Animation("arrow.png", 6), pos, dest)
  arrow.freeze()

  val dirArray = Array(new Point(1, 0), new Point(0, 1), new Point(-1, 1), new Point(-1, 0), new Point(0, -1), new Point(1, -1))
  var currentDir = 0
  val name = "cursor"
  var limitation = false  // indicates if the cursor is restricted to highlighted tiles
  var visible = true      // indicates if the cursor is currently visible

  def rotate(rot:Int) = 
  {
    currentDir = (currentDir + rot + dirArray.size) % dirArray.size
    arrow.currentFrame = (arrow.currentFrame + rot + dirArray.size) % dirArray.size
  }

  def getDir(dir:Int):Point = 
  {
    dir match
    {
      case 1  => return dirArray(currentDir)
      case -1 => return dirArray((currentDir + 3) % dirArray.size)
    }
  }
  override def show() = 
  {
    if(visible && Game.player.curAP > 0)
    {
      arrow.show()
      super.show()
    }
  }

  override def move(dir:Point) =
  {
    val nextX = pos.x + dir.x
    val nextY = pos.y + dir.y
    val next = new Point(nextX, nextY)
    if (Map.isInbound(next) && Map.fromPoint(next).isVisible() && Map.fromPoint(next).walkable && (!limitation || Map.fromPoint(next).isHighlighted()))
    {
      Map.fromPoint(pos).select(false)
      pos.add(dir)
      Map.fromPoint(pos).select(true)
    }
    else 
      findNext(dir)
  }

  def findNext(dir:Point):Unit =
  {
    // If the highlighted zone is not connex we must try to find the next component
    var i = pos.x+dir.x
    var j = pos.y+dir.y
    var p = new Point(i, j)
    while(Map.isInbound(p))
    {
      if (Map.fromPoint(p).isVisible() && (!limitation || Map.fromPoint(p).isHighlighted()))
      {
          setPos(p)
          return
      }
      i += dir.x
      j += dir.y
      p = new Point(i ,j)
    }
  }

  def setPos(dest:Point)
  {
    Map.fromPoint(pos).select(false)
    pos.setPoint(dest)
    Map.fromPoint(pos).select(true)
  }

}

import upickle.default._
import json._
object Player
{

  implicit val rw: ReadWriter[Player] =
      readwriter[ujson.Value].bimap[Player](
        e => JsonTools.write(e),
        json => read[Player](json)
    )
}
class Player() 
    extends SentientEntity(new Animation("character.png", 4, sizeY=64), new Point(0,0)) with Serializable
{
    val name = "Player"
        inventory = new Inventory(this, MessageHandler.inventory)

    var maxHP = 100
    var curHP = 100
    
    var maxSanity = 100
    var sanity = 100
    
    var armorClass = 30
    
    var baseAP = 5
    var modifAP = 0
    var curAP = baseAP
    
    var baseStr = 5
    var modifStr = 0
    
    var baseDex = 100
    var modifDex = 0

    var basePow = 100
    var modifPow = 0
    
    var seeRange = 9
    var modifSee = 0


    var weapon:Weapon = WeaponCreator.create("shotgun")

    def loot()
    {
    }

    override def kill()
    {
      // TODO
    }

    override def damage(dam:Int, from:SentientEntity):Unit = 
    {
      super.damage(dam, from)
      displayInfo()
    }
    override def heal(hp:Int):Unit=
    {
      super.heal(hp)
      displayInfo()
    }
    override def applyMagicCost(cost:Int):Unit =
    {
      super.applyMagicCost(cost:Int)
      displayInfo()
    }
    override def applyEffects():Unit =
    {
      super.applyEffects()
      displayInfo()
    }
    def dodge():Boolean = {return false}

    def getSeeRange():Int = 
    {
        return seeRange + modifSee
    }

    def attack(dest:Point):Unit =
    {
      if(curAP > 0) {
        curAP = 0
        Game.currentWeapon.attack(dest, this, Game.cursor.currentDir)
      }
      displayInfo()
    }

    def displayInfo():Unit = 
    {
      val zone = MessageHandler.playerInfo
      zone.clear()
      zone.addMessage("Gold:%d".format(gold))
      zone.addMessage("HP:%d/%d\t\tArmor:%d".format(curHP, maxHP, armorClass))
      zone.addMessage("AP:%d/%d(+%d)\t\tSanity:%d/%d".format(curAP, baseAP, modifAP, sanity, maxSanity))
      zone.addMessage("Str:%d(+%d)\t\t\tDex:%d(+%d)".format(baseStr, modifStr, baseDex, modifDex))
      zone.addMessage("Pow:%d(+%d)\t\t\tWeight:%d/%d".format(basePow, modifPow, curWeight, maxWeight))
      zone.addMessage("Helmet:%s(+%d)\t\tChesplate:%s(+%d)".format(helmet.name, helmet.armorClass, chestplate.name, chestplate.armorClass))
      zone.addMessage("Leggings:%s(+%d)\t\tBoots:%s(+%d)".format(leggings.name, leggings.armorClass, boots.name, boots.armorClass))
      zone.addMessage("Weapon:%s".format(weapon.name))
    }
}

class Inventory(val owner:SentientEntity, val zone:MessageZone=MessageHandler.tradeZone)
{
    var inventory:Vector[Item] = Vector() 
    var invStart = 0  // index of first element to be displayed
    var invSize = 10  // number of element to display at once
    var curInv = 0    // index of currently selected item
    var nbItem = 0    // number of item in inventory

    def display():Unit =
    {
      zone.clear()
      var i = 0
      for(j <- inventory)
      {
        if (invStart <= i && i < invStart+invSize)
        {
          if (i == curInv){
            zone.addMessage("> "+j.getInfo())
            MessageHandler.setItemInfo(j.getDescription())
          }
          else
            zone.addMessage(j.getInfo())
        }
        i+=1
      }
      zone.show()
    }
    def prevPage():Unit =
    {
        if (invStart != 0)
        {
            curInv -= invSize
            invStart -= invSize
        }
        display()
    }
    def nextPage():Unit =
    {
        if (invStart+invSize < nbItem)
        {
            curInv += invSize
            invStart += invSize
        }
        display()
    }
    def moveItem(d:Int):Unit =
    {
      if (invStart <= curInv + d && curInv + d < nbItem.min(invStart + invSize))
        curInv += d
      display()
    }
    def useItem():Unit =
    {
      inventory(curInv).onUse(owner)
      display()
    }
    def remove(i:Item):Unit =
    {
      inventory = inventory.filterNot(_ == i)
      nbItem -= 1
      owner.curWeight -= i.weight
      curInv = curInv.min(nbItem -1)
      display()
    }
    def add(i:Item):Unit=
    {
      inventory = inventory :+ i
      nbItem += 1
      owner.curWeight += i.weight
      display()
    }
    def drop():Unit =
    {
      inventory(curInv).pos.setPoint(owner.pos)
      Map.fromPoint(owner.pos).placeItem(inventory(curInv), Some(owner))
    }
    def sell(dest:SentientEntity):Unit =
    {
      dest.inventory.add(inventory(curInv))
      dest.gold -= inventory(curInv).price
      owner.gold += inventory(curInv).price
      remove(inventory(curInv))
    }
}
