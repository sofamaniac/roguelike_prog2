package position

class Point(xc:Int, yd:Int)
{
  var x = xc
  var y = yd
  def add(b:Point) = {
    x = b.x + x
    y = b.y + y
  }
}
