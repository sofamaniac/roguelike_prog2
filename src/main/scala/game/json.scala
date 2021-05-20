package json

import upickle.default._
import scala.io.Source
import enemy._
import graphics._
import position._
import item._
import weapon._
import entity._
import enemy._
import map._
import animation._
import scala.util.Random
import scala.collection.mutable.{Map => MapObject}

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

  def loadMap[Key, Value](json:ujson.Value)(implicit rwv: ReadWriter[Value], rwk: ReadWriter[Key]):MapObject[Key, Value]=
  {
    var res = MapObject[Key, Value]()
    var i = 0
    for (i <- 0 to length(json)-1)
    {
      println(json(i)(0), json(i)(1))
      res += upickle.default.read[Key](json(i)(0)) -> upickle.default.read[Value](json(i)(1))
    }
    return res
  }

  def loadVect[T](json:ujson.Value)(implicit rw: ReadWriter[T]):Vector[T]=
  {
    var res = Vector[T]()
    var i = 0
    for (i <- 0 to length(json) -1)
    {
      res = res :+ upickle.default.read[T](json(i))
    }
    return res
  }

  import scala.reflect.ClassTag
  import scala.reflect._

  def writeType[T](obj:T):String=
  {
    obj match
    {
      case e:Int => upickle.default.write(e)
      case e:String => upickle.default.write(e)
      case e:Boolean => upickle.default.write(e)
      case e:Animation => upickle.default.write(e)
      case e:Weapon => upickle.default.write(e)
      case e:Item => upickle.default.write(e)
      case e:Tile => upickle.default.write(e)
      case e:Room => upickle.default.write(e)
      case e:Merchant => { println(obj.getClass.getName); upickle.default.write(e)}
      case e:CowardNPC => upickle.default.write(e)
      case e:NeutralNPC =>  upickle.default.write(e)
      case e:Player => upickle.default.write(e)
      case e:Point => upickle.default.write(e)
      case e:Enemy => upickle.default.write(e)
      case e:GraphicEntity => upickle.default.write(e)
      case e:LootTable => upickle.default.write(e)
      case _ => throw new IllegalArgumentException("writeType called with type : " + obj.getClass.getName)
    }
  }
  def write[T: ClassTag](obj:T, attributes:List[String]=null)(implicit rw:ReadWriter[T]):String=
  {
    var fields:List[java.lang.reflect.Field] = null
    if (attributes == null)
      fields = classTag[T].runtimeClass.getDeclaredFields.toList
    else
      fields = attributes.map( (n:String) => classTag[T].runtimeClass.getDeclaredField(n) )
    val couples = fields.map{ n => 
        n.setAccessible(true)
        val res = "\"" + n.getName + "\": "+ writeType(n.get(obj))
        n.setAccessible(false)
        res.replace("\\\\", "")
      }
    return "{" + couples.mkString(",") + "}"
  }

  def sanitize(json:String):String=
    return json.replace("\\", "").replace("\"{", "{").replace("}\"", "}")
      .replace("\"[", "[").replace("]\"", "]").replace("[\"", "[").replace(",\",", ",")
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

object RoomCreator{
  val path = "src/main/ressources/data/"
  val data = Source.fromFile(path+"room.json").getLines.mkString

  def create(name:String=""):Room={
    Room.nameToCreate = name
    val result = read[Room](data)
    Room.nameToCreate = ""
    return result
  }
}
