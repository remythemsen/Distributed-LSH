package multiprobing

import hashfunctions.{HashFunctionLong, HyperplaneLong}
import measures.Distance

import scala.collection.mutable

class PQProbeGenerator(k:Int, hfs:Array[HyperplaneLong]) extends ProbeKeyGenerator {
  // ((idOfRepetition, generatedKey), score)

  object Ord extends Ordering[((Int, Long), Double)] {	// not implicit
    def compare(x: ((Int, Long), Double), y: ((Int, Long), Double)) = y._2.compare(x._2)
  }

  val pq = new mutable.PriorityQueue[((Int, Long), Double)]()(Ord)
  val epsilon = 0.01
  var dotProducts:Array[Double] = new Array(k)

  override def generate(qp:Array[Float]) : Unit = {
    pq.clear
    // Compute each set of probes from each key
    var hashIndex, hyperIndex, i, j = 0
    var oneStepHash:Long = 0
    var twoStepHash:Long = 0
    var hash:Long = 0

    while (hashIndex < hfs.length) {
      // Get dot products between qp and hyperplane for each k hyperplanes in hashFunction
      while(hyperIndex < hfs(i).hyperPlanes.length) {
        this.dotProducts(hyperIndex) = Distance.dotProduct(qp, hfs(hashIndex).hyperPlanes(hyperIndex))
        hyperIndex += 1
      }

      // Make key itself
      hash = hfs(hashIndex)(qp)
      pq += Tuple2(Tuple2(hashIndex, hash),Double.PositiveInfinity) // The key itself has top priority

      while(i < k) { // 1-Step Probes

        oneStepHash = checkAndFlip(hash, i)
        pq += Tuple2(Tuple2(hashIndex, oneStepHash), Math.pow(epsilon, -Math.abs(this.dotProducts(i))))

        j = i+1
        while(j < k) { // 2-Step Probes
          twoStepHash = checkAndFlip(oneStepHash, j)
          pq += Tuple2(Tuple2(hashIndex, twoStepHash), Math.pow(epsilon, -Math.abs(this.dotProducts(i))-Math.abs(this.dotProducts(j))))
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
/*



      var oneStepProbe: Long = 0

      // Adding the key itself
      val key = (kIndex, keys(i)._2)

      probes(c) = key
      c += 1

      while (i < k) {
        probes(c) = (kIndex, checkAndFlip(key._2, i))
        oneStepProbe = probes(c)._2

        c = c + 1
        j = i + 1
        while (j < k) {
          probes(c) = (kIndex, checkAndFlip(oneStepProbe, j))
          c = c + 1
          j = j + 1
        }
        i += 1
      }

      kIndex += 1
    }

    this.probesLeft = probes.length

*/




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
