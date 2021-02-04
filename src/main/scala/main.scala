import scalafx.animation.AnimationTimer
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.stage.Stage
import scalafx.scene.Scene
import scalafx.scene.Group
import scalafx.scene.layout.Pane
import scalafx.scene.canvas.Canvas
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.scene.text.FontWeight
import scalafx.scene.text.FontPosture;
import scalafx.scene.paint.Color._
import scalafx.scene.image.Image
import scalafx.scene.image._
/*
object HelloStageDemo extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title.value = "Hello Stage"
    width = 600
    height = 450
    scene = new Scene {
      fill = LightGreen
      content = new Rectangle {
        x = 25
        y = 40
        width = 100
        height = 100
        fill <== when (hover) choose Green otherwise Red
      }
    }
  }
}
*/

/*
object ImageTest extends JFXApp {

  val ressource_folder = "file:src/main/ressources/"
  stage = new JFXApp.PrimaryStage{}
  stage.setTitle("Test Image")
  stage.setHeight(650)
  stage.setWidth(500)

  val im = new ImageView(ressource_folder + "dome.png")

  val scene = new Scene{
    content = im
  }
  scene.fill = Black
  stage.setScene(scene)
  stage.show()
}
*/

object ExtendedTest extends JFXApp {
  val canvasH = 600
  val canvasW = 800

  val gameCanvas = new Canvas(canvasW, canvasH)

  val ressource_folder = "file:src/main/ressources/"
  val image = new ImageView(ressource_folder + "dome.png")
  val gc = gameCanvas.graphicsContext2D

  stage = new JFXApp.PrimaryStage
  stage.height = canvasH
  stage.width = canvasW 
  val scene = new Scene
  scene.fill = Black
  scene.root = new Pane {children = List(gameCanvas)}
  scene.content = image

  stage.setScene(scene)

  val animateTimer = AnimationTimer { t=>

    image.setRotate(image.getRotate() + 2)
  }
  val animateTimer2 = AnimationTimer { t=>

    image.setRotate(image.getRotate() -3)
  }
  animateTimer.start()
  animateTimer2.start()
}
