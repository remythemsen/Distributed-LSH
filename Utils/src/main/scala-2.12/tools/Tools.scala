package tools

import scala.collection.mutable

object Tools {

  def dotProduct(x:Array[Float], y:Array[Float]) : Double = {
    var i = 0
    var res:Double = 0.0
    while(i < x.length) {
      res += x(i) * y(i)
      i+=1
    }
    res
  }

  def dotProduct[A](x:A, y:A) : Double = {
    ???
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
