package hashfunctions
import tools.Tools._
import scala.util.Random

case class Hyperplane(k: Int, seed:Long, numOfDim: Int) extends HashFunction[Array[Float]](k, seed, numOfDim) {
  private val rnd:Random = new Random(seed)
  private val numberOfDimensions:Int = numOfDim
  val hyperPlanes:Array[Array[Float]] = generateHyperplanes(numberOfDimensions, k)
  private val probes:Array[Long] = new Array(1+(k*(k+1)/2))
  private val hashDotResult:Array[Double] = Array(k)
  override val state: Array[Array[Float]] = hyperPlanes

  def hashDot(v: Array[Float]):Array[Double] = {
    var i = 0
    while(i < hashDotResult.length) {
      hashDotResult(i) = dotProduct(v, hyperPlanes(i))
      i+=1
    }
    hashDotResult
  }

  override def apply(v: Array[Float]): Long = {
    var result:Long = 0
    var i = 0
    while(i < k) {
      result += (hash(v, hyperPlanes(i)) << i)
      i+=1
    }
    result
  }

  // TODO Change this into Breeze dotproduct
  // TODO Remove the branch if possible
  private def hash(v: Array[Float], randomV: Array[Float]): Long = {
    if (dotProduct(v, randomV) > 0) 1 else 0
  }

  def generateHyperplanes(size:Int, k:Int) : Array[Array[Float]] = {
    val hyperPlanes = new Array[Array[Float]](k)
    var i = 0
    while(i < hyperPlanes.length) {
      hyperPlanes(i) = Array.fill[Float](size)(rnd.nextGaussian.toFloat)
      i+=1
    }
    hyperPlanes
  }
}
