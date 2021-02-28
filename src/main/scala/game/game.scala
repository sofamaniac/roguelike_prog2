package game

import enemy._
import entity._
import item._
import map._
import position._
import graphics._
import scalafx.scene.input.KeyCode

object Game
{
    val player = new Player(GameWindow.contextGame)
    val cursor = new Cursor(GameWindow.contextGame)
    var currentPhase = ""
    setPhase("move", true)

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
        case _        => ()
      }
    }

    def setPhase(phase:String, isSelectionPhase:Boolean) = 
    {
      if(isSelectionPhase && phase != currentPhase)
      {
        if(!Map.fromPoint(cursor.pos).highlight)
        {
          cursor.setPos(player.pos)
        }
      }
      if(phase == "move")
      {
        Map.setHighlight(0, player.curAP, player.pos)
        cursor.limitation = true
      }
      else if(phase == "attack")
      {
        Map.setHighlight(player.weapon.innerRange, player.weapon.outerRange, player.pos)
        cursor.limitation = true
        if(!Map.fromPoint(cursor.pos).highlight)
        {
          cursor.pos.setPoint(Map.findHighlight())
        }
      }
      else if(phase == "info")
      {
        cursor.limitation = false
        Map.setHighlight(-1, -1, player.pos)
      }
      else if(phase == "inventory")
      {
        player.curInv = 0
      }
      if (phase != "inventory")
      {
        player.curInv = -1
      }
      currentPhase = phase
    }

    def handleSelection() =
    {
        currentPhase match
        {
            case "move"   => player.move(cursor.pos)
                             setPhase("move", true)
                             if(player.curAP < 1)
                             {
                                 loop()
                                 setPhase("move", true)
                             }

            case "attack" => MessageHandler.clear()
                             player.attack(cursor.pos)
                             loop()
                             setPhase("move", true)
            case "info"   => ()
      }
    }

    def handleArrow(event:String):Unit = 
    {
      if(currentPhase == "inventory")
      {
        event match
        {
          case "Right"  => player.nextInv()
          case "Left"   => player.prevInv()
          case "Up"     => player.moveInv(-1)
          case "Down"   => player.moveInv(1)
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
    }

    def initialization() =
    {
        // generate map : already done for now
        player.move(new Point(0, 0))
        MessageHandler.clear()
        player.displayInventory()

        // creating and placing enemies :
        enemiesVector = enemiesVector :+ new MeleeEnemy(new Point(5,5), GameWindow.contextGame, "Cultist Brawler", 100, 100, 30, 5, 0, 10, 0, 10, 0, 99, 0, 0, new MeleeWeapon("OldKnife", 0, 0, 0, 0, 4, 2))
        enemiesVector(0).move(enemiesVector(0).pos)

        // creating and placing items :
    }

    def loop() = 
    {
        player.curAP = player.baseAP + player.modifAP
        player.displayInventory()
        // ennemis morts -> array.filter (hp > 0) !!!
        enemiesVector.foreach
        {
            case _ =>
        }
    }
}
