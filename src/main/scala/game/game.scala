package game

import server._
import client._

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
import scala.collection.mutable.{Map => MapObject}

object GameServer
{
  var playerVect:MapObject[String, Player] = MapObject[String, Player]()
  var hasEndedTurn:MapObject[String, Boolean] = MapObject()
  var enemiesVector:Vector[Enemy] = Vector()
  var player = new Player()
  val cursor = new Cursor(GameWindow.contextGame)
  var currentPhase = ""
  var currentWeapon = player.weapon
  var speakingTo:SentientEntity = player

  def createPlayer(id:String):Unit =
  {
    playerVect += id -> new Player()
    playerVect(id).pos.setPoint(new Point(1, 1))
    hasEndedTurn += id -> false
    Map.fromPoint(new Point(1, 1)).entity = Some(playerVect(id))
  }

  def initialization():Unit = 
  {
    player.pos.setPoint(new Point(1, 1))
    Map.fromPoint(new Point(1,1)).entity = Some(player)
    enemiesVector = Map.getEnemies()

  }

  def pickUp():Unit =
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

  def loop():Unit=
  {
    var end = true
    hasEndedTurn.foreach{ case (i,b) => end = end && b}
    if(!end) return

    playerVect.foreach{ case (i,p) => p.endTurn()}
    hasEndedTurn.foreach{ case (i,p) => hasEndedTurn(i) = false}

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


    if(player.curHP <= 0)
    {
      // TODO:: change
      // for now on game over, the game is just reset
      player.curHP = player.maxHP
    }
    Map.update()  // We update the rooms of the map

    Server.sendAction("END_TURN")

  }

  def speak():Unit =
  {
    Map.fromPoint(cursor.pos).entity match
    {
      case Some(e) => if(player.pos.distance(e.pos) <= 1)
                      {
                        speakingTo = e
                      }
      case _       => ()
    }
  }
  def trade():Unit =
  {
    Map.fromPoint(cursor.pos).entity match
    {
      case Some(e) => if(player.pos.distance(e.pos) <= 1)
                      {
                        speakingTo = e
                      }
      case _       => ()
    }
  }

  def sell():Unit =
  {
    player.inventory.sell(speakingTo)
  }
}

object GameClient
{
    val player = new Player()
    val cursor = new Cursor(GameWindow.contextGame)
    var currentPhase = ""
    var currentWeapon = player.weapon
    var speakingTo:SentientEntity = player

    var enemiesVector:Vector[Enemy] = Vector()

    def eventHandler(kc:KeyCode) =
    {
      kc.getName match
      {
        case "Right" | "Left" | "Up" | "Down" => handleArrow(kc.getName)
        case "A"      => setPhase("attack")
        case "I"      => setPhase("info")
        case "Space"  => handleSelection()
        case "Esc"    => setPhase("move")
        case "E"      => setPhase("inventory")
        case "F"      => Client.sendAction(s"INVENTORY/DROP/${player.inventory.curInv}")//player.inventory.drop()
        case "G"      => pickUp()
        case "S"      => speak()
        case "T"      => trade()
        case "Enter"  => Client.sendAction("END_TURN")
        case "F1"     => MessageHandler.setHelp()
        case _        => ()
      }
    }

    def setPhase(phase:String) = 
    {
        cursor.visible = true
        var selectionPhase = true // Are we selcting a tile on the map
        if(phase == "move")
        {
          Map.setHighlight((p:Point)=>(player.pos.distance(p) <= player.curAP && player.curAP > 0), highlightPlayer=true)
            cursor.limitation = true
        }
        else if(phase == "attack" && player.curAP > 0)
        {
            setAttackHighlight()
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
        else if(phase == "inventory" || phase == "speak" || phase == "trade")
        {
          selectionPhase = false
          speakingTo = if(phase == "inventory") player else speakingTo
        }
        if(selectionPhase && phase != currentPhase)
        {
            if(!Map.fromPoint(cursor.pos).isHighlighted())
            {
              cursor.setPos(player.pos)
            }
            if(currentWeapon.zone != Zones.classic _) // other weapon zone should have the cursor on the player's tile
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
            case "move"   => Client.sendAction(s"MOVE/${cursor.pos.x},${cursor.pos.y}")
                             setPhase("move")

            case "attack" => MessageHandler.clear()
                             Client.sendAction(s"ATTACK/${cursor.pos.x},${cursor.pos.y},${cursor.currentDir}/${currentWeapon.name}")
                             setPhase("move")

            case "info"   => ()
            case "inventory" => Client.sendAction(s"INVENTORY/USE/${player.inventory.curInv}")
            case "speak" => Client.sendAction(s"SPEAK/USE/${cursor.pos.x},${cursor.pos.y}:${speakingTo.inventory.curInv}")
            case "trade"  => sell()
            case _ => println(currentPhase)
      }
    }

    def handleArrow(event:String):Unit = 
    {
      if(currentPhase == "inventory" || currentPhase == "speak" || currentPhase == "trade")
      {
        val entity = if(currentPhase == "speak") speakingTo else player
        event match
        {
          case "Right"  => entity.inventory.nextPage()
          case "Left"   => entity.inventory.prevPage()
          case "Up"     => entity.inventory.moveItem(-1)
          case "Down"   => entity.inventory.moveItem(1)
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
        // update attack zone when rotation
        setAttackHighlight()
      }

    }

    def setAttackHighlight():Unit =
    {

        // Zones.classic is different because it attack only on tile, but we need to select which one
        if (currentWeapon.zone == "classic")
        {
          Map.setHighlight((p:Point)=>p.distance(player.pos) >= currentWeapon.innerRange && p.distance(player.pos) <= currentWeapon.outerRange, true)
        }
        else
          Map.setHighlight((p:Point)=>currentWeapon.getZone()(currentWeapon.innerRange, currentWeapon.outerRange, cursor.currentDir, player.pos, p), true)
    }

    def initialization() =
    {
        MessageHandler.clear()
        // generate map : already done for now
        player.pos.setPoint(new Point(1, 1))
        Map.fromPoint(new Point(1,1)).entity = Some(player)
        Client.sendAction(s"INVENTORY/ADD/${WeaponCreator.create().name}")

        setPhase("move")
        player.inventory.display()
        player.inventory.curInv = 0

        // creating and placing enemies :
        enemiesVector = Map.getEnemies()

        // creating and placing items :
        MessageHandler.clear()
    }

    def loop() = 
    {
        player.endTurn()
        player.inventory.display()
        speakingTo.inventory.display()

        changeWeapon(player.weapon)

        enemiesVector = enemiesVector.filter(_.curHP > 0) // We remove enemies killed by the player
        enemiesVector.foreach
        { e =>
            e.curAP = e.baseAP + e.modifAP
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

        setPhase(currentPhase)    // ensure highlight is up to date

        if(player.curHP <= 0)
        {
          // for now on game over, the game is just reset
          player.curHP = player.maxHP
          initialization()
        }
        Map.update()  // We update the rooms of the map
        player.displayInfo() // We update the text on screen to update the player's status
    }

    def changeWeapon(weapon:Weapon):Unit=
    {
      currentWeapon = weapon
      setPhase(currentPhase)
    }

    def pickUp():Unit =
    {
      Map.fromPoint(player.pos).item match
      {
        case None    => ()
        case Some(i) => if (player.curWeight + i.weight <= player.maxWeight)
                        {
                          /*
                          player.inventory.add(i)
                          Map.fromPoint(player.pos).item = None
                          */
                         Client.sendAction(s"INVENTORY/PICKUP/${player.pos.x},${player.pos.y}")
                        }
      }
    }

    def speak():Unit =
    {
      Map.fromPoint(cursor.pos).entity match
      {
        case Some(e) => if(player.pos.distance(e.pos) <= 1)
                        {
                          speakingTo = e
                          setPhase("speak")
                        }
        case _       => ()
      }
    }
    def trade():Unit =
    {
      Map.fromPoint(cursor.pos).entity match
      {
        case Some(e) => if(player.pos.distance(e.pos) <= 1)
                        {
                          speakingTo = e
                          setPhase("trade")
                        }
        case _       => ()
      }
    }

    def sell():Unit =
    {
      Client.sendAction(s"INVENTORY/SELL/${player.inventory.curInv}:${speakingTo.pos.x},${speakingTo.pos.y}")
    }
}

