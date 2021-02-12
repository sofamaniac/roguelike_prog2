package game

import scalafx.application.JFXApp
import scalafx.stage.Stage
import scalafx.scene.Group
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.canvas.GraphicsContext
import scalafx.animation.AnimationTimer

object Game
{
  val window = new JFXApp.PrimaryStage
  val width = 800
  val height = 600
  window.height = height
  window.width  = width
  
  val groupMenu = new Group
  val groupGame = new Group
  val sceneMenu = new Scene(groupMenu)
  val sceneGame = new Scene(groupGame)

  val canvasGame = new Canvas(width, height)
  val canvasMenu = new Canvas(width, height)
  
  groupMenu.getChildren().add(canvasMenu)
  groupGame.getChildren().add(canvasGame)

  window.setScene(sceneGame)
}
