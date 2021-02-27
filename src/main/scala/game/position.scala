package position

class Point(var x:Int, var y:Int)
{
    def add(b:Point) =
    {
        x = b.x + x
        y = b.y + y
    }

    def distance(b:Point):Int =
    {
        return ((x - b.x).abs + (y - b.y).abs + (y + x - b.y - b.x).abs) / 2
    }
}
