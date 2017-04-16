package hashfunctions

import scala.collection.mutable
import scala.util.Random

case class BitHash(k:Int, seed:Long, dimensions:Int) {

  private val randomIndices:Array[Int] = generateRandomIndices(dimensions)

  def apply(v: mutable.BitSet): Long = {
    var result:Long = 0
    var i = 0
    while(i < k) {
      result += (hash(v, randomIndices(i)) << i)
      i += 1
    }
    result
  }

  def hash(v: mutable.BitSet, index:Int) : Int = {
    if(v(index)) 1 else 0
  }

  def generateRandomIndices(dimensions:Int) : Array[Int] = {
    val rnd = new Random(seed)
    val result = new Array[Int](k)

    for(i <- 0 until k) {
      result(i) = rnd.nextInt(dimensions)
    }
    result
  }

}
