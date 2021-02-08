import graphics._
import scalafx.application.JFXApp

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

  val test = new Test
  test.test()
}
