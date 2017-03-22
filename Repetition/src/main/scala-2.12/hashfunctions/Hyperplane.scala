package hashfunctions

import measures.Distance
import scala.util.Random

case class Hyperplane(k: Int, rndf:() => Random, numOfDim: Int) extends HashFunction {
  private val rnd:Random = rndf()
  private val numberOfDimensions:Int = numOfDim
  private val hyperPlanes = for {
    _ <- (0 until k).toArray
    hp <- Array(generateRandomV(numberOfDimensions))
  } yield hp

  private val probes = new Array[Array[Int]]((k*k)/2) // Array of probes to be reused

  // TODO Change to use long
  override def apply(v: Array[Float]): Array[Int] = {
    val result = new Array[Int](k)
    // For performance, a while loop is used
    var i = 0
    while(i < k) {
      result(i) = hash(v, hyperPlanes(i))
      i+=1
    }
    result
  }

  // TODO Change this into Breeze dotproduct
  private def hash(v: Array[Float], randomV: Array[Float]): Int = {
    // TODO Done use parallel for this dot p
    if (Distance.parDotProduct(v, randomV) > 0) 1 else 0
  }

  /**
    * Generates random hyperplane, speed here is not essential
    * Since these are computed in the preprocessing step
    * @param size
    * @return random hyperplane
    */
  def generateRandomV(size: Int) : Array[Float] = {
    val set = for {
      _ <- (0 until size).toArray
      c <- Array[Float]({
        if (rnd.nextBoolean()) -1 else 1
      })
    } yield c

    set
  }

  /**
    * 2-step multiprobe scheme
    * Generates a set of keys from a vector
    *
    * @param v
    * @return
    */

  override def generateProbes(v: Array[Float]): Array[Array[Int]] = {
    // TODO update to long
    val hashCode:Array[Int] = apply(v)
    var i,j,c = 0
    while(i < k) {
      System.arraycopy(hashCode, 0, probes(c), 0, k) // Copies values from hashCode into existing array in probes
      // TODO remove this assignment
      val OneStepProbe = probes(c)(i) = 1 - probes(c)(i) // efficient flip (here we permute)

      c = c+1 // c is updated to copy the reference into probes array index
      j = i+1
      while(j < k) {
        System.arraycopy(OneStepProbe, 0, probes(c), 0, k)
        // using i'th permute to generate the j set
        probes(c)(j) = 1 - probes(c)(j)
        c = c+1
        j = j+1
      }
      i+=1
    }
    this.probes // probes has been update in place
  }
}
