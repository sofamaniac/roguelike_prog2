package entity

import graphics._
import scalafx.Includes._
import scalafx.scene.image._
import scalafx.scene.canvas._
import position._

abstract class Entity(animation:Array[ImageView], pos:Point, dest:GraphicsContext) 
  extends GraphicEntity(animation:Array[ImageView], pos:Point, dest:GraphicsContext)
{
  val name : String
  def move(dir:Point):Unit
}

abstract class SentientEntity(animation:Array[ImageView], pos:Point, dest:GraphicsContext) 
  extends Entity(animation:Array[ImageView], pos:Point, dest:GraphicsContext)
{
  var max_hp:Int// health points
  var hp:Int    // current hp
  var wis:Int   // wisdom
  var kno:Int   // knowledge
  var cons:Int  // constitution
  var char:Int  // charisma
  
  def attack()
  def speak()
  def loot() // Generat loot on death
}

class Player(dest:GraphicsContext)
  extends SentientEntity(AnimationLoader.load_animation("ressources/player_animation"), new Point(0,0), dest:GraphicsContext)
{
  val name="Player"
  var max_hp = 100
  var hp = 100
  var wis = 100
  var kno = 100
  var cons = 100
  var char = 100
  def move(dir:Point):Unit = 
  {
    pos.add(dir)
  }

  def attack()={}
  def speak()={}
  def loot={}
}
  
