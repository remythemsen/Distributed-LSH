package hashfunctions
import measures.Distance
import scala.util.Random

case class HyperplaneLong(k: Int, seed:Long, numOfDim: Int) extends HashFunctionLong {
  private val rnd:Random = new Random(seed)
  private val numberOfDimensions:Int = numOfDim
  val hyperPlanes:Array[Array[Float]] = generateHyperplanes(numberOfDimensions, k)
  private val probes:Array[Long] = new Array(1+(k*(k+1)/2))
  private val hashDotResult:Array[Double] = Array(k)
  override val state: Array[Array[Float]] = hyperPlanes

  def hashDot(v: Array[Float]):Array[Double] = {
    var i = 0
    while(i < hashDotResult.length) {
      hashDotResult(i) = Distance.dotProduct(v, hyperPlanes(i))
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
    if (Distance.dotProduct(v, randomV) > 0) 1 else 0
  }

  /**
    * Generates set of random hyperplanes
    * @param size
    * @return random hyperplane
    */
  def generateHyperplanes(size:Int, k:Int) : Array[Array[Float]] = {
    val hyperPlanes = new Array[Array[Float]](k)
    var i = 0
    while(i < hyperPlanes.length) {
      hyperPlanes(i) = Array.fill[Float](size)(rnd.nextGaussian.toFloat)
      i+=1
    }
    hyperPlanes
  }

  /**
    * 2-step multiprobe scheme
    * Generates a set of keys from a vector
    *
    * @param hashCode
    * @return
    */

  override def generateProbes(hashCode:Long): Array[Long] = {

    var i,c,j = 0
    var oneStepProbe:Long = 0

    // Adding the key itself
    probes(i) = hashCode
    c += 1

    while(i < k) {
      probes(c) = hashCode
      checkAndFlip(c, i)
      oneStepProbe = probes(c)

      c = c+1
      j = i+1
      while(j < k) {
        probes(c) = oneStepProbe
        checkAndFlip(c, j)
        c = c+1
        j = j+1
      }
      i+=1
    }

    def checkAndFlip(c:Int, i:Int): Unit = {
      if((probes(c) & (1 << i)) != 0) {
        probes(c) -= (1 << i)
      } else {
        probes(c) += (1 << i)
      }
    }

    probes
  }

}
