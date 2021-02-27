package game

import entity._
import map._
import position._
import graphics._
import scalafx.scene.input.KeyCode

object Game
{
    val player = new Player(GameWindow.contextGame)
    player.move(new Point(0,0))
    val cursor = new Cursor(GameWindow.contextGame)
    var currentPhase = ""
    setPhase("move", true)

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
        case _        => ()
      }
    }

    def setPhase(phase:String, isSelectionPhase:Boolean) = 
    {
      if(isSelectionPhase && phase != currentPhase)
      {
        cursor.setPos(player.pos)
      }
      if(phase == "move")
      {
        Map.setHighlight(0, player.curAP)
        cursor.limitation = true
      }
      else if(phase == "attack")
      {
        Map.setHighlight(player.weapon.innerRange, player.weapon.outerRange)
        cursor.limitation = true
        cursor.pos.setPoint(Map.findHighlight())
      }
      else if(phase == "info")
      {
        cursor.limitation = false
        Map.setHighlight(-1, -1)
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

        case "attack" => player.attack(cursor.pos)
                         loop()
                         setPhase("move", true)
        case "info"   => ()
      }
    }

    def initialization() =
    {
    }

    def loop() = 
    {
      player.curAP = player.baseAP + player.modifAP
      // resolve actions (dodge from ennemies)
      // ennemies turn
      // dodge for the player
    }
}
