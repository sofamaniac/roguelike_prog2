package json

import upickle.default._
import scala.io.Source
import enemy._
import graphics._
import position._
import item._
import map._

object EnemyCreator{

  val path = "src/main/ressources/data/"

  val data = Source.fromFile(path+"enemy.json").getLines.mkString

  def test():Unit={
    println(read[Enemy](data))
  }
}
