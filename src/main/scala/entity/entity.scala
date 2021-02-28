package entity

import graphics._
import scalafx.Includes._
import scalafx.scene.image._
import scalafx.scene.canvas._
import position._
import map._
import game._
import item._

abstract class Entity(animation:Array[ImageView], pos:Point, dest:GraphicsContext) 
    extends GraphicEntity(animation, pos, dest)
{
    val name:String
    def move(dir:Point):Unit
}

abstract class ControlledEntity(animation:Array[ImageView], pos:Point, dest:GraphicsContext)
  extends Entity(animation, pos, dest)
{
  val arrow = new GraphicEntity(AnimationLoader.load("arrow.png", 6), pos, dest)
  arrow.freeze()

  val dirArray = Array(new Point(1, 0), new Point(0, 1), new Point(-1, 1), new Point(-1, 0), new Point(0, -1), new Point(1, -1))
  var currentDir = 0

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

  def move(dir:Point):Unit = 
  {
    val nextX = pos.x + dir.x
    val nextY = pos.y + dir.y
    if (nextX >= 0 && nextY >= 0 && nextX < Map.tileArray.size && nextY < Map.tileArray(nextX).size)
    {
      pos.add(dir)
    }
  }
}

abstract class SentientEntity(animation:Array[ImageView], pos:Point, dest:GraphicsContext) 
    extends Entity(animation, pos, dest)
{
    val name:String
    var maxHP:Int           // health points
    var curHP:Int           // current hp

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
      return (next.x >= 0 && next.y >= 0 && next.x < Map.tileArray.size && next.y < Map.tileArray(next.y).size
            && Map.tileArray(next.x)(next.y).entity == None && Map.tileArray(next.x)(next.y).walkable)
    }


    def getInfo():String =
    {
      return "%s : %s/%s HP".format(name, curHP, maxHP)
    }

    def damage(dam:Int, from:SentientEntity):Unit=
    {
      // [from] can be used to apply a thorn-like effect
      curHP -= dam
      if(curHP <= 0)
      {
        Map.fromPoint(pos).entity = None
      }
    }

    def attack(dest:Point):Unit =
    {
        weapon.attack(dest, this)
    }

    def dodge():Boolean

    def loot() // Generate loot on death
}


class Cursor(dest:GraphicsContext)
  extends ControlledEntity(AnimationLoader.load("cursor.png", 1), new Point(0,0), dest)
{
  val name = "cursor"
  var limitation = false  // indicates if the cursor is restricted to highlighted tiles

  override def show() = 
  {
    arrow.show()
    super.show()
  }

  override def move(dir:Point) =
  {
    val nextX = pos.x + dir.x
    val nextY = pos.y + dir.y
    if (nextX >= 0 && nextY >= 0 && nextX < Map.tileArray.size && nextY < Map.tileArray(nextX).size
        && Map.tileArray(nextX)(nextY).isVisible() && (!limitation || Map.tileArray(nextX)(nextY).highlight))
    {
      Map.tileArray(pos.x)(pos.y).selected = false
      pos.add(dir)
      Map.tileArray(pos.x)(pos.y).selected = true
    }
  }

  def setPos(dest:Point)
  {
    Map.tileArray(pos.x)(pos.y).selected = false
    pos.setPoint(dest)
    Map.tileArray(pos.x)(pos.y).selected = true
  }

}

class Player(dest:GraphicsContext)
    extends SentientEntity(AnimationLoader.load("character.png", 4, sizeY=64), new Point(0,0), dest)
{
    val name = "Player"

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
    
    var seeRange = 5
    var modifSee = 0

    var weapon:Weapon = new MeleeWeapon("Bare Hands", 0, 0, 1, 1, 1, 4)
    var inventory:Vector[Item] = Vector()
    var invStart = 0  // index of first element to be displayed
    var invSize = 10  // number of element to display at once
    var curInv = 0    // index of currently selected item
    var nbItem = 0    // number of item in inventory

    inventory = inventory :+ new MeleeWeapon("Bare Hands", 0, 0, 1, 1, 1, 4)
    inventory = inventory :+ new MeleeWeapon("Fire Hands", 0, 0, 1, 1, 1, 4)
    nbItem += 2
    def loot()
    {

    }

    def dodge():Boolean = {return false}

    def getSeeRange():Int = 
    {
        return seeRange + modifSee
    }

    def displayInventory():Unit =
    {
      MessageHandler.clearInventory()
      var i = 0
      for(j <- inventory)
      {
        if (invStart <= i && i < invStart+invSize)
        {
          if (i == curInv) 
            MessageHandler.addInventory("> "+j.getInfo()) 
          else 
            MessageHandler.addInventory(j.getInfo())
        }
        i+=1
      }
      MessageHandler.show()
    }


    def prevInv():Unit =
    {
      if (invStart != 0)
        invStart -= invSize
      displayInventory()
    }
    def nextInv():Unit =
    {
      if (invStart+invSize < nbItem)
        invStart += invSize
      displayInventory()
    }
    def moveInv(d:Int):Unit =
    {
      if (invStart <= curInv + d && curInv + d < nbItem.min(invStart + invSize))
        curInv += d
      displayInventory()
    }
}
