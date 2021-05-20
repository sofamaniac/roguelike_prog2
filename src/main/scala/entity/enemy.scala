package enemy

import entity._
import item._
import weapon._
import position._
import scalafx.scene.canvas._
import scalafx.scene.image._
import graphics._
import animation._
import game._
import map._
import json._
import scala.util.{Random => Rand}

import scala.collection.mutable.{Map => MapObject}
import upickle.default._

object Enemy
{
  // We setup the default value for every parameter of enemy
  val defAnimation = Animation.loadDefault()
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
  val defWea = WeaponCreator.create("sword")
  val defLT = new LootTable()
  val defBeh = "classic"
  val defFly = false
  // we define how are object serialized, 
  implicit val rw: ReadWriter[Enemy] = 
    readwriter[ujson.Value].bimap[Enemy](
      e => JsonTools.write(e),
      json => createEnemy(json)
    )

  var nameToCreate = "" // name of enemy to create (rw can't take any parameter so we need to pass them using an attribute)

  def createEnemy(_json: ujson.Value):Enemy =
  {
    var json = _json
    var index = JsonTools.find(json, "name", nameToCreate)
    if (index == -1)
    {
      println("enemy.json: name %s not found.".format(nameToCreate))
      json = JsonTools.getRandom(json)
    }
    else
      json = json(index)
    
    var args = MapObject[String, Int]()

    val animation   = if (JsonTools.contains(json, "animation")) Animation.loadJson(json("animation")) else defAnimation
    val name        = JsonTools.load(json, "name", defName)
    args += "maxHP" -> JsonTools.load(json, "maxHP", defMHP)
    args += "armorClass" -> JsonTools.load(json, "armorClass", defAC)
    args += "baseAP" -> JsonTools.load(json, "baseAP", defBAP)
    args += "modifAP" -> JsonTools.load(json, "modifAP", defMAP)
    args += "baseStr" -> JsonTools.load(json, "baseStr", defBST)
    args += "modifStr" -> JsonTools.load(json, "modifStr", defMST) 
    args += "baseDex" -> JsonTools.load(json, "baseDex", defBDE)
    args += "modifDex" -> JsonTools.load(json, "modifDex", defMDE)
    args += "basePow" -> JsonTools.load(json, "basePow", defBPO)
    args += "modifPow" -> JsonTools.load(json, "modifPow", defMPO)
    val fly         = JsonTools.load(json, "fly", defFly)
    val weapon      = if (JsonTools.contains(json, "weapon")) WeaponCreator.create(json("weapon").str) else defWea
    val loot        = if (JsonTools.contains(json, "lootTable")) read[LootTable](json("lootTable")) else defLT
    val behaviour   = JsonTools.load(json, "behaviourType", defBeh)

    val res = behaviour match
    {
      case "merchant" => new Merchant(animation, new Point(8,8), name, fly, weapon, loot, args) // TODO: add possibility to define trades in json
      case "neutral"  => new NeutralNPC(animation, new Point(6,7), name, fly, weapon, loot, args)
      case "coward"   => new CowardNPC(animation, new Point(2,3), name, fly, weapon, loot, args)
      case _          => new Enemy(animation, new Point(5, 3), name, fly, weapon, loot, args)
    }
    return handleAdditionalParameters(res, json)
  }

  import scala.reflect.ClassTag
  import scala.reflect._
  def handleAdditionalParameters(res:Enemy, json:ujson.Value):Enemy=
  {
    val addParam = List("curHP", "curAP", "curWeight", "maxWeight", "gold",  "onFireDuration", "onFireDamage", 
      "poisonDuration", "poisonDamage","paralyzedDuration", "paralyzedDamage", "frozenDuration", "frozenDamage")
    addParam.foreach( n =>
        if (JsonTools.contains(json, n))
        {
          val f = classTag[Enemy].runtimeClass.getDeclaredField(n)
          f.setAccessible(true)
          f.set(res, upickle.default.read[Int](json(n)))
          f.setAccessible(false)
        }
      )
    return res
  }

}
// I don't know why I need to overwrite pos and animation
// TODO: look up why it is the case
case class Enemy(override val animation:Animation, override val pos:Point, val name:String, var maxHP:Int, var armorClass:Int, var baseAP:Int, var modifAP:Int, var baseStr:Int, var modifStr:Int, var baseDex:Int, var modifDex:Int, var basePow:Int, var modifPow:Int, var fly:Boolean, var weapon:Weapon, var lootTable:LootTable)
    extends SentientEntity(animation, pos)
{
  def this(animation:Animation, pos:Point, name:String, fly:Boolean, weapon:Weapon, lootTable:LootTable, map:MapObject[String, Int])=
  {
    this(animation, pos, name, map("maxHP"), map("armorClass"), map("baseAP"), map("modifAP"), map("baseStr"), map("modifStr"), map("baseDex"), map("modifDex"), map("basePow"),
          map("modifPow"), fly, weapon, lootTable)
  }
  var curHP = maxHP
  var curAP = baseAP

  def IA():Unit =
  {
    val next = findBestMove()
    move(next)
    var i = 0
    for(i <- 0 until dirArray.size)
    {
      if(curAP > 0 && weapon.getZone()(weapon.innerRange, weapon.outerRange, i, pos, Game.player.pos))
      {
        // launch the attack to the player position, so adjacent tiles can be affected to
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
            if(weapon.getZone()(weapon.innerRange, weapon.outerRange, i, pos, Game.player.pos))
            {
                return pos
            }
        }
        // else find a position that move it closer to the player 
        // in the future, enemies will have like the player a detection range,
        // outside of which they are unable to see the player
        val curD = pos.distance(Game.player.pos)
        Map.map.rooms.foreach
        {
            case(key, r) =>
              r.tiles.foreach
              {
                case (k, t) =>
                  if(t.coord.distance(Game.player.pos) < curD && isMoveValid(t.coord))
                    return t.coord
              }
        }
        return pos
  }
  def dodge():Boolean = {return false}
  def loot():Unit = 
  {
    val item = lootTable.loot()
    item.pos.setPoint(pos)
    Map.fromPoint(pos).placeItem(item)
  }
}

object NeutralNPC
{
  implicit val rw: ReadWriter[NeutralNPC] =
    readwriter[ujson.Value].bimap[NeutralNPC](
      e => { val s = upickle.default.write(e.asInstanceOf[Enemy]); s.dropRight(2) + """, "behaviourType":"neutral"}"""" },
      json => Enemy.createEnemy(json).asInstanceOf[NeutralNPC]
    )
}

class NeutralNPC(animation:Animation, pos:Point, name:String, fly:Boolean, weapon:Weapon, loot:LootTable, map:MapObject[String, Int])
  extends Enemy(animation, pos, name, fly, weapon, loot, map)
{
  var neutral:Boolean = true
  
  override def IA()
  {
    if (!neutral || lastHitBy == Game.player)
    {
      neutral = false
      super.IA()
    }
  }
}

object CowardNPC
{
  implicit val rw: ReadWriter[CowardNPC] =
    readwriter[ujson.Value].bimap[CowardNPC](
      e => { val s = upickle.default.write(e.asInstanceOf[Enemy]); s.dropRight(2) + """, "behaviourType":"coward"}""" },
      json => Enemy.createEnemy(json).asInstanceOf[CowardNPC]
    )
}

class CowardNPC(animation:Animation, pos:Point, name:String, fly:Boolean, weapon:Weapon, loot:LootTable, map:MapObject[String,Int])
  extends Enemy(animation, pos, name, fly, weapon, loot, map)
{
  // Coward NPCs cannot attack and run away from the player

  override def IA():Unit=
  {
    // TODO: run away only when player is visible
    
    // The entity look for a tile that will increase its distance from the player
    val curD = pos.distance(Game.player.pos)
    Map.map.rooms.foreach
    {
        case(key, r) =>
          r.tiles.foreach
          {
            case(k, t) =>
            if(t.coord.distance(Game.player.pos) >= curD && isMoveValid(t.coord))
                  move(t.coord)
          }
    }
  }
}


object Merchant
{
  implicit val rw: ReadWriter[Merchant] =
    readwriter[ujson.Value].bimap[Merchant](
      e => { val s = upickle.default.write(e.asInstanceOf[Enemy]); s.dropRight(2) + """, "behaviourType":"merchant"}""" },
      json => Enemy.createEnemy(json).asInstanceOf[Merchant]
    )
}
class Merchant(animation:Animation, pos:Point, name:String, fly:Boolean, weapon:Weapon, loot:LootTable, map:MapObject[String, Int])
  extends NeutralNPC(animation, pos, name, fly, weapon, loot, map)
{
  inventory = new Inventory(this){
    override def useItem():Unit =
    {
      Game.player.inventory.add(inventory(curInv))
      Game.player.gold -= inventory(curInv).price
      println(inventory(curInv).price)
      remove(inventory(curInv))
    }
  }
  inventory.add(ItemCreator.create("shotgun"))
  inventory.add(ItemCreator.create("bandages"))
  inventory.add(ItemCreator.create("gem"))
}


object LootTable {

  implicit val rw: ReadWriter[LootTable] = 
    readwriter[ujson.Value].bimap[LootTable](
      e => {var res = "["; e.table.foreach { case (n, w) => res += s"{item:$n, weight:$w}," }; res + "]," },
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
  var totalWeight:Int = 1

  def loot():Item = {

    val r = Rand.nextInt(totalWeight)
    var s = 0
    var i = 0

    while (s < r){
      s += table(i)._2
      i += 1
    }
    i = i.max(0).min(table.length - 1)
    return ItemCreator.create(table(i)._1)
  }

  def addItem(item:String, weight:Int):Unit = {
    table = table :+ ((item, weight))
    totalWeight += weight
  }
}
