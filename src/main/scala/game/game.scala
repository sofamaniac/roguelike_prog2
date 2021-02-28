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
        case "Right"  => cursor.rotate(1)
        case "Left"   => cursor.rotate(-1)
        case "Up"     => cursor.move(cursor.getDir(1))
        case "Down"   => cursor.move(cursor.getDir(-1))
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
        Map.setHighlight((p:Point)=>(player.pos.distance(p) <= player.curAP))
        cursor.limitation = true
      }
      else if(phase == "attack")
      {
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
      else if(phase == "inventory")
      {

      }
      currentPhase = phase
    }

    def handleSelection() =
    {
        currentPhase match
        {
            case "move"         => player.move(cursor.pos)
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

    def initialization() =
    {
        // generate map : already done for now
        player.move(new Point(0, 0))
        MessageHandler.clear()

        // creating and placing enemies :
        enemiesVector = enemiesVector :+ new MeleeEnemy(new Point(5,5), GameWindow.contextGame, "Cultist Brawler", 100, 100, 30, 5, 0, 10, 0, 10, 0, 99, 0, 0, new Weapon("Ice Blow", 1000000, 5, "pow", Zones.cone, 3, 0, 8, 5, 8))
        enemiesVector(0).move(enemiesVector(0).pos)

        // creating and placing items :
    }

    def loop() = 
    {
        player.curAP = player.baseAP + player.modifAP
        // ennemis morts -> array.filter (hp > 0) !!!
        enemiesVector.foreach
        {
            case _ =>
        }
    }
}
