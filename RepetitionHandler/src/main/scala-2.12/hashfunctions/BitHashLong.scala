package hashfunctions

import scala.util.Random


case class BitHashLong(k:Int, dimensions:Int, seed:Long) extends HashFunctionLong {

  private val randomIndices:Array[Int] = generateRandomIndices(dimensions)
  override val state: Array[Array[Float]] = ???
  private var result:Long = 0

  override def apply(v: Array[Boolean]): Long = {
    var i = 0
    while(i < k) {
      result += (hash(v, randomIndices(i)) << i)
      i += 1
    }
    result
  }

  def hash(v:Array[Boolean], index:Int) : Long = {

  }

  override def generateProbes(key: Long): Array[Long] = ???

  def generateRandomIndices(dimensions:Int) : Array[Int] = {
    val rnd = new Random(seed)

    for(i <- 0 until k) {
      this.randomIndices(i) = rnd.nextInt(dimensions)
    }
  }

}
