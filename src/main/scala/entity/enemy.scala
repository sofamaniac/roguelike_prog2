package enemy

import entity._
import item._
import position._
import scalafx.scene.canvas._
import scalafx.scene.image._
import graphics._
import animation._
import animation.Animation.Animation
import game._
import map._
import json._
import scala.util.{Random => Rand}

import upickle.default._
object Enemy
{
  // We setup the default value for every parameter of enemy
  val defAnimation = Animation.load("goblin.png", 11, sizeY=58)
  val defName = ""
  val defMHP = 100
  val defAC = 30
  val defBAP = 5
  val defMAP = 0
  val defBST = 10
  val defMST = 0
  val defBDE = 10
  val defMDE = 0
  val defBPO = 10
  val defMPO = 0 
  val defWea = new Weapon("Cone Weapon example", 1000000, 5, "pow", Zones.cone, 1, 0, 8, 5, 8)
  val defLT = new LootTable()
  val defBeh = "classic"
  val defFly = false
  // we define how are object serialized, 
  // for instance here, only the name and the maxHP would be save to Json
  // whereas, we create a new Enemy using only the first field of the json as a name
  implicit val rw: ReadWriter[Enemy] = 
    readwriter[ujson.Value].bimap[Enemy](
      e=> ujson.Arr(e.name, e.maxHP),
      json => createEnemy(json)
    )


  var nameToCreate = "" // name of enemy to create (rw can't take any parameter so we need to pass them using an attribute)

  def createEnemy(_json: ujson.Value):Enemy =
  {
    var json = _json
    var index = JsonTools.find(json, "name", nameToCreate)
    if (index == -1)
      json = JsonTools.getRandom(json)
    else
      json = json(index)


    val animation   = if (JsonTools.contains(json, "animation")) Animation.loadJson(json) else defAnimation   // TODO: to complete
    val name        = JsonTools.load(json, "name", defName)
    val maxHP       = JsonTools.load(json, "maxHP", defMHP)
    val armorClass  = JsonTools.load(json, "armorClass", defAC)
    val baseAP      = JsonTools.load(json, "baseAP", defBAP)
    val modifAP     = JsonTools.load(json, "modifAP", defMAP)
    val baseStr     = JsonTools.load(json, "baseStr", defBST)
    val modifStr    = JsonTools.load(json, "modifStr", defMST) 
    val baseDex     = JsonTools.load(json, "baseDex", defBDE)
    val modifDex    = JsonTools.load(json, "modifDex", defMDE)
    val basePow     = JsonTools.load(json, "basePow", defBPO)
    val modifPow    = JsonTools.load(json, "modifPow", defMPO)
    val fly         = JsonTools.load(json, "fly", defFly)
    val weapon      = defWea   // TODO: to complete
    val loot        = if (JsonTools.contains(json, "lootTable")) read[LootTable](json("lootTable")) else defLT
    val behaviour   = JsonTools.load(json, "behaviourType", defBeh)
    // TODO differentiate based on behaviour
    return Enemy(animation, new Point(5, 3), name, maxHP, armorClass, baseAP, modifAP, baseStr, modifStr, baseDex, modifDex, basePow, modifPow, fly, weapon, loot)
  }
}
// I don't know why I need to overwrite pos and animation
// TODO: look up why it is the case
case class Enemy(override val animation:Animation, override val pos:Point, val name:String, var maxHP:Int, var armorClass:Int, var baseAP:Int, var modifAP:Int, var baseStr:Int, var modifStr:Int, var baseDex:Int, var modifDex:Int, var basePow:Int, var modifPow:Int, var fly:Boolean, var weapon:Weapon, var lootTable:LootTable)
    extends SentientEntity(animation, pos)
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
    val item = lootTable.loot()
    item.pos.setPoint(pos)
    Map.fromPoint(pos).item = Some(item)
  }
}

object LootTable {

  implicit val rw: ReadWriter[LootTable] = 
    readwriter[ujson.Value].bimap[LootTable](
      e=> ujson.Arr(e.totalWeight),
      json => createTable(json)
    )
  
  def createTable(json:ujson.Value):LootTable = {
    val result = new LootTable
    JsonTools.foreach(json,
      (j:ujson.Value) => result.addItem(j("item").str, j("weight").num.toInt)
      )
    return result
  }
}

case class LootTable() {

  // We store the tuple (itemName, weight)
  // an item is create only when loot is called
  var table:Vector[(String, Int)] = Vector()
  var totalWeight:Int = 0

  def loot():Item = {

    val r = Rand.nextInt(totalWeight)
    var s = 0
    var i = 0

    while (s < r){
      s += table(i)._2
      i += 1
    }
    return ItemCreator.create(table(i)._1)
  }

  def addItem(item:String, weight:Int):Unit = {
    table = table :+ ((item, weight))
    totalWeight += weight
  }
}
