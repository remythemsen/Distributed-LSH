package hashfunctions

import measures.Distance

import scala.util.Random

case class Crosspolytope(k: Int, seed:Long, numOfDim: Int) extends HashFunction {
  private val rnd:Random = new Random(seed)
  private val numberOfDimensions:Int = numOfDim

  // TODO How many
  private val probes:Array[Array[Int]] = {
    val a = new Array[Array[Int]]((k*(k+1)/2)+1) // Array of probes to be reused
    for(i <- 0 until a.length) {
      a(i) =  new Array[Int](k)
    }
    a
  }


  // TODO Change to use long
  override def apply(v: Array[Float]): Array[Int] = {
    Array()

  }

  override def generateProbes(hashCode: Array[Int]): Array[Array[Int]] = {
    // TODO update to long

    this.probes // probes has been update in place
  }
}
