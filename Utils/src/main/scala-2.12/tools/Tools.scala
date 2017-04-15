package tools

/**
  * Created by remeeh on 15-04-2017.
  */
object Tools {

  def dotProduct(x:Array[Float], y:Array[Float]) : Float = {
    var i = 0
    var res:Float = 0f
    while(i < x.length) {
      res += x(i) * y(i)
      i+=1
    }
    res
  }

  def magnitude(x: Array[Float]) : Double = {
    var r:Double = 0.0
    var i = 0
    while(i < x.length) {
      r+=Math.pow(x(i), 2)
      i += 1
    }

    Math.sqrt(r)
  }

}
