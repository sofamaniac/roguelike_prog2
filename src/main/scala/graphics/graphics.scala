package graphics

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

class Test
{
  def test ():Unit =
  {
      val ressource_folder = "file:src/main/ressources/"
      val image = new ImageView(ressource_folder + "dome.png")
      val imag2 = new ImageView(ressource_folder + "shipGreen.png")

      val stage = new JFXApp.PrimaryStage
      stage.height = 600
      stage.width =  800

      val root  = new Group
      val scene = new Scene(root)
      val canvas = new Canvas(800, 600)
      root.getChildren().add(canvas)
      val context = canvas.getGraphicsContext2D
      
      context.setFill(Black)
      // val entity = new DisplayEntity(image, new Point(100, 200), context)
      val entit2 = new DisplayEntity(imag2, new Point(200, 100), context)
      stage.setScene(scene)
      val animation = new AnimatedEntity(Array(image, imag2), new Point(100, 200), context)
      
      val animateTimer = AnimationTimer
      {

          t=>context.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
             // entity.show()
             // entity.move(new Point(1, 0))
             // entit2.show()
             animation.show()
      }
      animateTimer.start()
  }
}

class Point(xc:Int, yd:Int)
{
  var x = xc
  var y = yd
  def add(b:Point) = {
    x = b.x + x
    y = b.y + y
  }
}

class DisplayEntity(im_p: ImageView, pos_p:Point, dest_p:GraphicsContext)
{
  var im = im_p
  val pos = pos_p
  var dest = dest_p
  def show() : Unit =
  {
    dest.drawImage(im.getImage(), pos.x, pos.y)
  }
  def move(tr:Point):Unit=
  {
    pos.add(tr)
  }
}

class AnimatedEntity(animation_p: Array[ImageView], pos_p: Point, dest_p: GraphicsContext) extends DisplayEntity(animation_p(0), pos_p, dest_p)
{
  val animation = animation_p
  var animationDuration : Int = 60 // duration in frame of the entire cycle
  var currentFrame = 0
  var frameLength = animationDuration / animation.size
  var frameCounter = 0 // keep count of how many frames the current frame has been displayed

  override def show() : Unit =
  {
    super.show()
    frameCounter += 1
    if (frameCounter > frameLength)
    {
      currentFrame = (currentFrame + 1) % animation.size
      frameCounter = 0
      im = animation(currentFrame)
    }
  }
  def setAnimationDuration(t: Int) : Unit = 
  {
    animationDuration = t
    frameLength = animation.size / t
  }
}


