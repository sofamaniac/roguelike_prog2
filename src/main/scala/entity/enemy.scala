package enemy

import entity._
import item._
import position._
import scalafx.scene.canvas._
import graphics._

class MeleeEnemy(pos:Point, dest:GraphicsContext, name:String, maxHp:Int, hp:Int, armorClass:Int, baseSpd:Int, modifSpd:Int, baseStr:Int, modifStr:Int, baseDex:Int, modifDex:Int, var weapon:MeleeWeapon)
    extends SentientEntity(AnimationLoader.load("ressources/melee_enemy_animation", 1), pos, dest)
{
    def attack() = 
    {
        // roll 1d100
        // if roll > AC_enemy -> touch
        // roll damage
    }
    def speak() = 
    {
        // free action, once per turn
    }
    def loot() =
    {
        
    }
}

class RangeEnemy(pos:Point, dest:GraphicsContext, name:String, maxHp:Int, hp:Int, armorClass:Int, baseSpd:Int, modifSpd:Int, baseStr:Int, modifStr:Int, baseDex:Int, modifDex:Int, var weapon:MeleeWeapon)
    extends SentientEntity(AnimationLoader.load("ressources/melee_enemy_animation", 1), pos, dest)
{
    def attack() = 
    {
        // roll 1d100
        // if roll > AC_enemy -> touch
        // roll damage
    }
    def speak() = 
    {
        // free action, once per turn
    }
    def loot() =
    {

    }
}

class CasterEnemy(pos:Point, dest:GraphicsContext, name:String, maxHp:Int, hp:Int, armorClass:Int, baseSpd:Int, modifSpd:Int, baseStr:Int, modifStr:Int, baseDex:Int, modifDex:Int, var weapon:MeleeWeapon)
    extends SentientEntity(AnimationLoader.load("ressources/melee_enemy_animation", 1), pos, dest)
{
    def attack() = 
    {
        // roll 1d100
        // if roll > AC_enemy -> touch
        // roll damage
    }
    def speak() = 
    {
        // free action, once per turn
    }
    def loot() =
    {

    }
}
// Définir les caractéristiques des ennemis (caractéristiques, path vers les animations, ...) dans un format
// de fichier type Json, cependant json n'est par défaut pas supporté par Scala
