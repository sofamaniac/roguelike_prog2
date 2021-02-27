package position

class Point(xc:Int, yd:Int)
{
  var x = xc
  var y = yd
  def add(b:Point) = {
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
}
