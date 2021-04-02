package json

import upickle.default._
import scala.io.Source
import enemy._
import graphics._
import position._
import item._
import weapon._
import map._
import scala.util.Random

object JsonTools{
  def length(json:ujson.Value):Int={
    var i = 0
    try {
      while (json(i) == json(i))
        i += 1
    }
    catch {
      case e: java.lang.IndexOutOfBoundsException => i
    }
    return i
  }

  def contains(json:ujson.Value, key:String):Boolean={
    try {
        val v = json(key)
        return true
    }
    catch {
      case e: java.util.NoSuchElementException => false
    }
  }

  def findGeneric(json:ujson.Value, condition:(Int=>Boolean)):Int = {
    var i = 0
    for (i<-0 to length(json)-1) {
      if (condition(i))
        return i
    }
    return -1
  }

  def find(json:ujson.Value, key:String, value:String):Int = {
    return findGeneric(json, (i:Int) => json(i)(key).str == value)
  }
  def find(json:ujson.Value, key:String, value:Int):Int = {
    return findGeneric(json, (i:Int) => json(i)(key).num == value)
  }
  def find(json:ujson.Value, key:String, value:Boolean):Int = {
    return findGeneric(json, (i:Int) => json(i)(key).bool == value)
  }

  def load[A](json:ujson.Value, key:String, default:A):A = {
    if (contains(json, key))
      default match{
        case s: String  => json(key).str.asInstanceOf[A]
        case i: Int     => json(key).num.toInt.asInstanceOf[A]
        case b: Boolean => json(key).bool.asInstanceOf[A]
      }
    else
      return default
  }

  def getRandom(json:ujson.Value):ujson.Value = {
    return json(scala.util.Random.nextInt(length(json)))
  }

  def foreach(json:ujson.Value, f:(ujson.Value=>Unit)):Unit = {
    var i = 0
    for (i <- 0 to length(json) -1) {
      f(json(i))
    }
  }


}

object EnemyCreator{

  val path = "src/main/ressources/data/"

  val data = Source.fromFile(path+"enemy.json").getLines.mkString

  def create(name:String=""):Enemy={
    Enemy.nameToCreate = name
    val result = read[Enemy](data)
    Enemy.nameToCreate = "" // reset to the default value
    return result
  }
}

object ItemCreator{
  val path = "src/main/ressources/data/"
  val data = Source.fromFile(path+"item.json").getLines.mkString

  def create(name:String=""):Item={
    Item.nameToCreate = name
    val result = read[Item](data)
    Item.nameToCreate = ""
    return result
  }
}

object WeaponCreator{
  val path = "src/main/ressources/data/"
  val data = Source.fromFile(path+"weapon.json").getLines.mkString
  def create(name:String=""):Weapon={
    Weapon.nameToCreate = name
    val result = read[Weapon](data)
    Weapon.nameToCreate = ""
    return result
  }
}