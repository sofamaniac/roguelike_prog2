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
            Map.setHighlight((p:Point)=>(player.pos.distance(p) <= player.curAP))
            cursor.limitation = true
        }
        else if(phase == "attack")
        {
            val p = Map.findHighlight()
            if(p.x == -1)
            {
                cursor.setPos(player.pos)
            }
            else
            {
                cursor.setPos(p)
            }
            Map.setHighlight((p:Point)=>player.weapon.zone(player.weapon, cursor.currentDir, player.pos, p))
            cursor.limitation = true
            if(!Map.fromPoint(cursor.pos).highlight)
            {
            cursor.pos.setPoint(player.pos)
            }
        }
        else if(phase == "info")
        {
            cursor.limitation = false
            Map.setHighlight((p:Point)=>false)
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
          Map.setHighlight((p:Point)=>player.weapon.zone(player.weapon, Game.cursor.currentDir, player.pos, p))
      }
    }

    def initialization() =
    {
        // generate map : already done for now
        player.move(new Point(0, 0))
        setPhase("move", true)
        MessageHandler.clear()
        player.inventory.display()
        player.inventory.curInv = 0

        // creating and placing enemies :
        enemiesVector = enemiesVector :+ new MeleeEnemy(new Point(5,5), GameWindow.contextGame, "Cultist Brawler", 100, 100, 30, 5, 0, 10, 0, 10, 0, 99, 0, 0, new Weapon("Ice Blow", 1000000, 5, "pow", Zones.cone, 3, 0, 8, 5, 8))
        enemiesVector(0).move(enemiesVector(0).pos)

        // creating and placing items :
    }

    def loop() = 
    {
        player.curAP = player.baseAP + player.modifAP
        player.inventory.display()
        // ennemis morts -> array.filter (hp > 0) !!!
        enemiesVector.foreach
        {
            case _ =>
        }
    }

    def pickUp() =
    {
      Map.fromPoint(player.pos).item match
      {
        case None    => ()
        case Some(i) => player.inventory.add(i)
                        Map.fromPoint(player.pos).item = None
      }
    }
}
