class hexadrillage(size:Int,max_id:Int){
    var pos_to_id = Array.ofDim[Int](size,size)
    /* the array of positions indexed by id. If an id is not in the grid, its corresponding position is -1,-1*/
    var id_to_pos = Array[Int](max_id)
    def int_to_tuple(n:Int) :Tuple2[Int,Int] = {
        n match {
            case -1 => (-1,1)
            case _ => (n/size,n%size)
        }
    }
    def tuple_to_int(t:Tuple2[Int,Int]):Int = {
        t match {
            case (-1,-1) => -1
            case _ => t._1 * size + t._2
        }
    }
    def add_entity(x:Int,y:Int,id:Int) = {
        pos_to_id(x)(y) = id
        id_to_pos(id) = tuple_to_int(x,y) 
    }
    def look_at_position(x:Int,y:Int) :Int = {
        return pos_to_id(x)(y)
    }
    def position_from_id(id:Int) :Tuple2[Int,Int] = {
        return int_to_tuple(id_to_pos(id))
    }
    /*from a given position, return the position located at a given distance and angle*/
    def look_to_direction(x:Int,y:Int,angle:Int,distance:Int) :Tuple2[Int,Int] = {
        angle match {
            case 0 => (x+distance,y)
            case 1 => (x+distance,y+distance)
            case 2 => (x,y+distance)
            case 3 => (x-distance,y)
            case 4 => (x-distance,y-distance)
            case 5 => (x,y-distance)
        }
    }
    def remove_entity(id:Int) = {
        val t = int_to_tuple (id_to_pos(id))
        pos_to_id(t._1)(t._2)=0
        id_to_pos(id)= -1
    }

}