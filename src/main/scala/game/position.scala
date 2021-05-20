package position

import game._
import item._
import weapon._
import map._


import upickle.default._
import json._
object Point
{
  implicit val rw: ReadWriter[Point] = 
    readwriter[ujson.Value].bimap[Point](
      p => s"""{"x":${p.x},"y":${p.y}}""",
      json => new Point(JsonTools.load(json, "x", 0), JsonTools.load(json, "y", 0))
    )
}

case class Point(var x:Int, var y:Int)
{
  def this(p:Point)
  {
    // this allow to copy a point
    this(p.x, p.y)
  }
    def add(b:Point) =
    {
        x = b.x + x
        y = b.y + y
    }

  def sub(b:Point) =
  {
    x = x - b.x
    y = y - b.y
  }

  def distance(b:Point):Int =
  {
    val dx = (x - b.x).abs
    val dy = (y - b.y).abs
    return ((x - b.x).abs + (y - b.y).abs + (y + x - b.y - b.x).abs) / 2
  }

  def setPoint(b:Point):Unit =
  {
    x = b.x
    y = b.y
  }
  def equals(p:Point):Boolean =
  {
    return x == p.x && y == p.y
  }
}

object Zones
{
    type definition = (Int, Int, Int, Point, Point) => Boolean
    def ring(minRange:Int, maxRange:Int, dir:Int, start:Point, dest:Point):Boolean =
    {
        val d = start.distance(dest)
        (minRange <= d) && (d <= maxRange) && Map.inSight(start, dest)
    }

    def ray(minRange:Int, maxRange:Int, dir:Int, start:Point, dest:Point):Boolean =
    {
        val dx = dest.x - start.x
        val dy = dest.y - start.y
        val dz = -dx-dy
        dir match
        {
            case 0 => (dy == 0) && (dx >= 0) && (minRange <= dx.abs) && (dx.abs <= maxRange)
            case 1 => (dx == 0) && (dz <= 0) && (minRange <= dz.abs) && (dz.abs <= maxRange)
            case 2 => (dz == 0) && (dy >= 0) && (minRange <= dy.abs) && (dy.abs <= maxRange)
            case 3 => (dy == 0) && (dx <= 0) && (minRange <= dx.abs) && (dx.abs <= maxRange)
            case 4 => (dx == 0) && (dz >= 0) && (minRange <= dz.abs) && (dz.abs <= maxRange)
            case 5 => (dz == 0) && (dy <= 0) && (minRange <= dy.abs) && (dy.abs <= maxRange)
        }
    }

    def cone(minRange:Int, maxRange:Int, dir:Int, start:Point, dest:Point):Boolean =
    {
        val dx = dest.x - start.x
        val dy = dest.y - start.y
        val dz = -dx-dy
        dir match
        {
            case 0 => (dy <= 0) && (dz <= 0) && (minRange <= dx.abs) && (dx.abs <= maxRange)
            case 1 => (dx >= 0) && (dy >= 0) && (minRange <= dz.abs) && (dz.abs <= maxRange)
            case 2 => (dx <= 0) && (dz <= 0) && (minRange <= dy.abs) && (dy.abs <= maxRange)
            case 3 => (dy >= 0) && (dz >= 0) && (minRange <= dx.abs) && (dx.abs <= maxRange)
            case 4 => (dx <= 0) && (dy <= 0) && (minRange <= dz.abs) && (dz.abs <= maxRange)
            case 5 => (dx >= 0) && (dz >= 0) && (minRange <= dy.abs) && (dy.abs <= maxRange)
        }
    }

    def classic(minRange:Int, maxRange:Int, dir:Int, start:Point, dest:Point):Boolean =
    {
      val d = start.distance(dest)
      (dest.equals(Game.cursor.pos) || dest.equals(Game.player.pos)) && // 2nd case is here to allow enemy to use single tile weapon
        minRange <= d && d <= maxRange
    }

}
