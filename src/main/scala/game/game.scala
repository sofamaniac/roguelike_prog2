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
    player.move(new Point(0,0))
    val cursor = new Cursor(GameWindow.contextGame)
    var currentPhase = ""
    setPhase("move", true)
    var currentActor:ControlledEntity = cursor

    var enemiesArray:Vector[Enemy] = Vector()

    def eventHandler(kc:KeyCode)
    {
        kc.getName match
        {
            case "Right"  => cursor.rotate(1)
            case "Left"   => cursor.rotate(-1)
            case "Up"     => cursor.move(currentActor.getDir(1))
            case "Down"   => cursor.move(currentActor.getDir(-1))
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
            cursor.pos.setPoint(player.pos)
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
            case "attack" => () // TODO : implement attack
            case "info"   => ()
        }
    }

    def initialization()
    {
        // generate map : already done for now

        // creating and placing enemies :
        enemiesArray = enemiesArray :+ new MeleeEnemy(new Point(5,5), GameWindow.contextGame, "Cultist Brawler", 100, 100, 30, 5, 0, 10, 0, 10, 0, 99, new MeleeWeapon("OldKnife", 0, 0, 0, 0))
        enemiesArray(0).move(enemiesArray(0).pos)

        // creating and placing items :
    }

    def loop() =
    {

    }
}
