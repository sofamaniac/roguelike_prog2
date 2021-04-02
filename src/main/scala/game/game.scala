package game

import enemy._
import entity._
import item._
import weapon._
import map._
import position._
import graphics._
import messageHandler._
import scalafx.scene.input.KeyCode
import json._

object Game
{
    val player = new Player()
    val cursor = new Cursor(GameWindow.contextGame)
    var currentPhase = ""
    var currentWeapon = player.weapon

    var enemiesVector:Vector[Enemy] = Vector()

    def eventHandler(kc:KeyCode) =
    {
      kc.getName match
      {
        case "Right" | "Left" | "Up" | "Down" => handleArrow(kc.getName)
        case "A"      => setPhase("attack", true)
        case "I"      => setPhase("info", true)
        case "Space"  => handleSelection()
        case "Esc"    => setPhase("move", true)
        case "E"      => setPhase("inventory", false)
        case "F"      => player.inventory.drop()
        case "G"      => pickUp()
        case "Enter"  => loop()
        case "F1"     => MessageHandler.setHelp()
        case _        => ()
      }
    }

    def setPhase(phase:String, isSelectionPhase:Boolean) = 
    {
        cursor.visible = true
        if(phase == "move")
        {
            Map.setHighlight((p:Point)=>(0< player.pos.distance(p) && player.pos.distance(p) <= player.curAP))
            cursor.limitation = true
        }
        else if(phase == "attack")
        {
            Map.setHighlight((p:Point)=>player.weapon.zone(player.weapon.innerRange, player.weapon.range, cursor.currentDir, player.pos, p), true)
            // We set the selection of the case to attack for weapon with non-zero outerrange
            Map.setHighlight((p:Point)=>p.distance(player.pos) >= player.weapon.innerRange && p.distance(player.pos) <= player.weapon.outerRange, erase=false)
            val p = Map.findHighlight()
            if(p.x == -1) // if no solution is found
            {
                cursor.visible = false
            }
            else
            {
                cursor.setPos(p)
            }
            cursor.limitation = true
        }
        else if(phase == "info")
        {
            cursor.limitation = false // cursor can move freely on all visible tiles
            Map.setHighlight((p:Point)=>false)
        }
        if(isSelectionPhase && phase != currentPhase)
        {
            if(!Map.fromPoint(cursor.pos).isHighlighted())
            {
              cursor.setPos(player.pos)
            }
        }
        currentPhase = phase
    }

    def handleSelection() =
    {
        currentPhase match
        {
            case "move"   => player.move(cursor.pos)
                             setPhase("move", true)
                             //MessageHandler.clear()

            case "attack" => MessageHandler.clear()
                             player.attack(cursor.pos)
                             setPhase("move", true)

            case "info"   => ()
            case "inventory" => player.inventory.useItem()
            case _ => println(currentPhase)
      }
    }

    def handleArrow(event:String):Unit = 
    {
      if(currentPhase == "inventory")
      {
        event match
        {
          case "Right"  => player.inventory.nextPage()
          case "Left"   => player.inventory.prevPage()
          case "Up"     => player.inventory.moveItem(-1)
          case "Down"   => player.inventory.moveItem(1)
        }
      }
      else
      {
        event match
        {
          case "Right"  => cursor.rotate(1)
          case "Left"   => cursor.rotate(-1)
          case "Up"     => cursor.move(cursor.getDir(1))
          case "Down"   => cursor.move(cursor.getDir(-1))
        }
      }
      if(currentPhase == "attack")
      {
        // update attack when zone when rotation
        Map.setHighlight((p:Point)=>currentWeapon.zone(currentWeapon.innerRange, currentWeapon.range, cursor.currentDir, player.pos, p), true)
        Map.setHighlight((p:Point)=>p.distance(player.pos) >= currentWeapon.innerRange && p.distance(player.pos) <= currentWeapon.outerRange, erase=false)
      }

    }

    def initialization() =
    {
        // generate map : already done for now
        player.move(new Point(0, 0))
        player.inventory.add(WeaponCreator.create())
        player.inventory.add(WeaponCreator.create("Fire Ball"))
        //player.inventory.add(new Weapon("Ray Weapon example", "", 1000000, 5, "pow", Zones.ray, 1, 0, 8, 5, 8))
        //player.inventory.add(new Weapon("Single Weapon example", "", 1000, 5, "pow", Zones.classic, 1, 5, 8, 5, 8))
        player.inventory.add(new Bandages)
        setPhase("move", true)
        MessageHandler.clear()
        player.inventory.display()
        player.inventory.curInv = 0

        // creating and placing enemies :
        enemiesVector = Vector()
        enemiesVector = enemiesVector :+ EnemyCreator.create()
        enemiesVector(0).move(enemiesVector(0).pos)

        // creating and placing items :
    }

    def loop() = 
    {
        player.endTurn()
        player.inventory.display()

        currentWeapon = player.weapon

        enemiesVector = enemiesVector.filter(_.curHP > 0) // We remove enemies killed by the player
        enemiesVector.foreach
        { e =>
            e.curAP = e.baseAP + e.modifAP
            e.IA()
        }
        // We separate in case we add animation to display the damage done
        enemiesVector.foreach
        {
          e => e.applyEffects()
        }
        enemiesVector.foreach
        {
          e => e.endTurn()
        }
        enemiesVector = enemiesVector.filter(_.curHP > 0) // We remove enemies dying of other causes than the player

        setPhase(currentPhase, true)  // reset the pase to movement phase

        if(player.curHP <= 0)
        {
          // for now on game over, the game is just reset
          player.curHP = player.maxHP
          initialization()
        }
        player.displayInfo() // We update the text on screen to update the player's status
    }

    def changeWeapon(weapon:Weapon):Unit=
    {
      currentWeapon = weapon
    }

    def pickUp() =
    {
      Map.fromPoint(player.pos).item match
      {
        case None    => ()
        case Some(i) => if (player.curWeight + i.weight <= player.maxWeight)
                        {
                          player.inventory.add(i)
                          Map.fromPoint(player.pos).item = None
                        }
      }
    }
}