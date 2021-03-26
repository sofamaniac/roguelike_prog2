package json

import upickle.default._
import scala.io.Source
import enemy._
import graphics._
import position._
import item._
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
        return (json(key) == json(key))
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

  def load(json:ujson.Value, key:String, default:String):String = {
    if (contains(json, key))
      return json(key).str
    else
      return default
  }
  def load(json:ujson.Value, key:String, default:Int):Int = {
    if (contains(json, key))
      return json(key).num.toInt
    else
      return default
  }
  def load(json:ujson.Value, key:String, default:Boolean):Boolean = {
    if (contains(json, key))
      return json(key).bool
    else
      return default
  }

  def getRandom(json:ujson.Value):ujson.Value = {
    return json(scala.util.Random.nextInt(length(json)))
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
