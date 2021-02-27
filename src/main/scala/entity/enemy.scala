package enemy

import entity._
import item._
import position._
import scalafx.scene.canvas._
import scalafx.scene.image._
import graphics._

abstract class Enemy(animation:Array[ImageView], pos:Point, dest:GraphicsContext) extends SentientEntity(animation, pos, dest) {}

class MeleeEnemy(pos:Point, dest:GraphicsContext, val name:String, var maxHP:Int, var curHP:Int, var armorClass:Int, var baseAP:Int, var modifAP:Int, var curAP:Int, var baseStr:Int, var modifStr:Int, var baseDex:Int, var modifDex:Int, var weapon:Weapon)
    extends Enemy(AnimationLoader.load("melee_enemy.png", 1), pos, dest)
{
    def attack()
    {
        // roll 1d100
        // if roll > AC_enemy -> touch
        // roll damage
    }
    def speak()
    {
        // free action, once per turn
    }
    def loot()
    {
        
    }
    def dodge():Boolean = {return false}
}

class RangeEnemy(pos:Point, dest:GraphicsContext, val name:String, var maxHP:Int, var curHP:Int, var armorClass:Int, var baseAP:Int, var modifAP:Int, var curAP:Int, var baseStr:Int, var modifStr:Int, var baseDex:Int, var modifDex:Int, var weapon:Weapon)
    extends Enemy(AnimationLoader.load("melee_enemy.png", 1), pos, dest)
{
    def attack()
    {
        // roll 1d100
        // if roll > AC_enemy -> touch
        // roll damage
    }
    def speak()
    {
        // free action, once per turn
    }
    def loot()
    {

    }
    def dodge():Boolean = {return false}
}

class CasterEnemy(pos:Point, dest:GraphicsContext, val name:String, var maxHP:Int, var curHP:Int, var armorClass:Int, var baseAP:Int, var modifAP:Int, var curAP:Int, var baseStr:Int, var modifStr:Int, var baseDex:Int, var modifDex:Int, var weapon:Weapon)
    extends Enemy(AnimationLoader.load("melee_enemy.png", 1), pos, dest)
{
    def attack()
    {
        // roll 1d100
        // if roll > AC_enemy -> touch
        // roll damage
    }
    def speak()
    {
        // free action, once per turn
    }
    def loot()
    {

    }
    def dodge():Boolean = {return false}
}
// Définir les caractéristiques des ennemis (caractéristiques, path vers les animations, ...) dans un format
// de fichier type Json, cependant json n'est par défaut pas supporté par Scala
