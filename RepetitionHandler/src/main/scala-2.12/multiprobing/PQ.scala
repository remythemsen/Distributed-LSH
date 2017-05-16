package multiprobing

import hashfunctions.HashFunction
import tools.Tools._

import scala.collection.mutable

class PQ[A](k:Int, hfs:Array[HashFunction[A]]) extends ProbeScheme[A] {
  // ((idOfRepetition, generatedKey), score)

  object Ord extends Ordering[((Int, Long), Double)] {	// not implicit
    def compare(x: ((Int, Long), Double), y: ((Int, Long), Double)):Int = x._2.compare(y._2)
  }

  val pq = new mutable.PriorityQueue[((Int, Long), Double)]()(Ord)
  var priorities:Array[Double] = new Array(k)

  override def generate(qp:A) : Unit = {
    pq.clear
    // Compute each set of probes from each key
    var hashIndex, hyperIndex, i, j = 0
    var oneStepHash:Long = 0
    var twoStepHash:Long = 0
    var hash:Long = 0

    while (hashIndex < hfs.length) {
      // Get dot products between qp and hyperplane for each k hyperplanes in hashFunction
      while(hyperIndex < hfs(i).state.length) {
        this.priorities(hyperIndex) = Math.exp(-Math.abs(dotProduct(qp, hfs(hashIndex).state(hyperIndex))))
        hyperIndex += 1
      }

      // Make key itself
      hash = hfs(hashIndex)(qp)
      pq += Tuple2(Tuple2(hashIndex, hash),Double.PositiveInfinity) // The key itself has top priority

      while(i < k) { // 1-Step Probes

        oneStepHash = checkAndFlip(hash, i)
        pq += Tuple2(Tuple2(hashIndex, oneStepHash), this.priorities(i))

        j = i+1
        while(j < k) { // 2-Step Probes
          twoStepHash = checkAndFlip(oneStepHash, j)
          pq += Tuple2(Tuple2(hashIndex, twoStepHash), this.priorities(i) * this.priorities(j))
          j += 1
        }
        i += 1
      }

      hashIndex += 1
      i = 0
      j = 0
      hyperIndex = 0
    }

    def checkAndFlip(key:Long, i:Int): Long = {
      var res:Long = key
      if((res & (1 << i)) != 0) {
        res -= (1 << i)
      } else {
        res += (1 << i)
      }
      res
    }

  }


  /**
    * Gets the next key in priorityQueue with highest score
    * (Key most like to have get good candidates)
    * @return
    */
  override def next(): (Int,Long) = {
    this.pq.dequeue()._1
  }

  override def hasNext(): Boolean = this.pq.nonEmpty
}
