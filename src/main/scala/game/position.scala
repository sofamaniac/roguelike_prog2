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
    return dx + dy
  }
}
