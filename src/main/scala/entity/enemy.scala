package enemy

import entity._
import item._

class MeleeEnemy(var pos:Point, val dest:GraphicsContext, val name:String, var maxHp:Int, var hp:Int, var armorClass:Int, val baseSpd:Int, var modifSpd:Int, val baseStr:Int, var modifStr:Int, val baseDex:Int, var modifDex:Int, weapon:MeleeWeapon)
    extends SentientEntity(AnimationLoader.load_animation("ressources/melee_enemy_animation"), pos, dest)
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

class RangedEnemy(var pos:Point, val dest:GraphicsContext, val name:String, var maxHp:Int, var hp:Int, var armorClass:Int, val baseSpd:Int, var modifSpd:Int, val baseStr:Int, var modifStr:Int, val baseDex:Int, var modifDex:Int, weapon:RangedWeapon)
    extends SentientEntity(AnimationLoader.load_animation("ressources/melee_enemy_animation"), pos, dest)
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

class CasterEnemy(var pos:Point, val dest:GraphicsContext, val name:String, var maxHp:Int, var hp:Int, var armorClass:Int, val baseSpd:Int, var modifSpd:Int, val baseStr:Int, var modifStr:Int, val baseDex:Int, var modifDex:Int, weapon:CasterWeapon)
    extends SentientEntity(AnimationLoader.load_animation("ressources/melee_enemy_animation"), pos, dest)
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
