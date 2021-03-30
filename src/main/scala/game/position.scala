package position

import game._
import item._
import weapon._
import map._

class Point(var x:Int, var y:Int)
{
    def add(b:Point) =
    {
        x = b.x + x
        y = b.y + y
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
    type definition = (Weapon, Int, Point, Point) => Boolean
    def ring(weapon:Weapon, dir:Int, start:Point, dest:Point):Boolean =
    {
        val d = start.distance(dest)
        (weapon.innerRange <= d) && (d <= weapon.range) && Map.inSight(start, dest)
    }

    def ray(weapon:Weapon, dir:Int, start:Point, dest:Point):Boolean =
    {
        val dx = dest.x - start.x
        val dy = dest.y - start.y
        val dz = -dx-dy
        dir match
        {
            case 0 => (dy == 0) && (dx >= 0) && (weapon.innerRange <= dx.abs) && (dx.abs <= weapon.range)
            case 1 => (dx == 0) && (dz <= 0) && (weapon.innerRange <= dz.abs) && (dz.abs <= weapon.range)
            case 2 => (dz == 0) && (dy >= 0) && (weapon.innerRange <= dy.abs) && (dy.abs <= weapon.range)
            case 3 => (dy == 0) && (dx <= 0) && (weapon.innerRange <= dx.abs) && (dx.abs <= weapon.range)
            case 4 => (dx == 0) && (dz >= 0) && (weapon.innerRange <= dz.abs) && (dz.abs <= weapon.range)
            case 5 => (dz == 0) && (dy <= 0) && (weapon.innerRange <= dy.abs) && (dy.abs <= weapon.range)
        }
    }

    def cone(weapon:Weapon, dir:Int, start:Point, dest:Point):Boolean =
    {
        val dx = dest.x - start.x
        val dy = dest.y - start.y
        val dz = -dx-dy
        dir match
        {
            case 0 => (dy <= 0) && (dz <= 0) && (weapon.innerRange <= dx.abs) && (dx.abs <= weapon.range)
            case 1 => (dx >= 0) && (dy >= 0) && (weapon.innerRange <= dz.abs) && (dz.abs <= weapon.range)
            case 2 => (dx <= 0) && (dz <= 0) && (weapon.innerRange <= dy.abs) && (dy.abs <= weapon.range)
            case 3 => (dy >= 0) && (dz >= 0) && (weapon.innerRange <= dx.abs) && (dx.abs <= weapon.range)
            case 4 => (dx <= 0) && (dy <= 0) && (weapon.innerRange <= dz.abs) && (dz.abs <= weapon.range)
            case 5 => (dx >= 0) && (dz >= 0) && (weapon.innerRange <= dy.abs) && (dy.abs <= weapon.range)
        }
    }

    def classic(weapon:Weapon, dir:Int, start:Point, dest:Point):Boolean =
    {
      dest.equals(Game.cursor.pos) || dest.equals(Game.player.pos) // 2nd case is here to allow enemy to use single tile weapon
    }

}
