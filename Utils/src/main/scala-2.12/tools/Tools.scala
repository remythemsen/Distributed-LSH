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
    // Not great!! TODO find a better solution
    var i = 0
    var res:Double = 0.0
    while(i < x.asInstanceOf[Array[Float]].length) {
      res += (x.asInstanceOf[Array[Float]](i) * y.asInstanceOf[Array[Float]](i))
      i+=1
    }
    res
  }

  def magnitudeD(x:Array[Double]) : Double = {
    var r = 0.0
    var i = 0
    while(i < x.length) {
      r+= x(i)*x(i)
      i+=1
    }
    Math.sqrt(r)
  }

  def magnitude(x: Array[Float]) : Double = {
    var r:Double = 0.0
    var i = 0
    while(i < x.length) {
      r+= x(i)*x(i)
      i += 1
    }

    Math.sqrt(r)
  }

}
