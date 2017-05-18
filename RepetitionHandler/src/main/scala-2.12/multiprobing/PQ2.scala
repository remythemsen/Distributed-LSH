package multiprobing

import hashfunctions.HashFunction
import tools.Tools._

import scala.collection.mutable

class PQ2[A](k:Int, hfs:Array[HashFunction[A]]) extends ProbeScheme[A] {
  // ((idOfRepetition, generatedKey), score)

  object Ord extends Ordering[((Int, Long), Double)] {	// not implicit
    def compare(x: ((Int, Long), Double), y: ((Int, Long), Double)):Int = y._2.compare(x._2)
  }
  object Ord2 extends Ordering[(mutable.HashSet[Int], Double)] {
    def compare(x: (mutable.HashSet[Int], Double), y: (mutable.HashSet[Int], Double)) = {
      y._2.compare(x._2)
    }
  }

  val pq = new mutable.PriorityQueue[((Int, Long), Double)]()(Ord)
  val pq2 = new mutable.PriorityQueue[(mutable.HashSet[Int], Double)]()(Ord2)
  var priorities:Array[Double] = new Array(k)
  var indices:Array[Int] = new Array(k)

  override def generate(qp:A) : Unit = {
    pq.clear
    pq2.clear
    // Compute each set of probes from each key
    var hashIndex, hyperIndex, i, j = 0
    var oneStepHash:Long = 0
    var twoStepHash:Long = 0
    var hash:Long = 0
    var maxProbesPerBucket = 100
    var elementsAdded = 0

    while (hashIndex < hfs.length) {
      // Get dot products between qp and hyperplane for each k hyperplanes in hashFunction
      while(hyperIndex < hfs(i).state.length) {
        var dot = dotProduct(qp, hfs(hashIndex).state(hyperIndex))
        this.priorities(hyperIndex) = dot * dot
        this.indices(hyperIndex) = hyperIndex
        hyperIndex += 1
      }
      this.indices = this.indices.sortWith((a, b) => this.priorities(a) < this.priorities(b))

      // Make key itself
      hash = hfs(hashIndex)(qp)
      pq += Tuple2(Tuple2(hashIndex, hash), 0.0) // The key itself has top priority
      var set = new mutable.HashSet[Int]()
      set += 0
      pq2 += Tuple2(set, this.priorities(this.indices(0)).toFloat)
      elementsAdded = 0
      while (!pq2.isEmpty && elementsAdded < maxProbesPerBucket) {
        elementsAdded += 1
        var e = pq2.dequeue
        var newHash = hash

        var priority = e._2
        var maxElem = -1
        for (elem <- e._1) {
          newHash = checkAndFlip(newHash, this.indices(elem))
          priority += this.priorities(this.indices(elem)).toFloat
          if (elem > maxElem) {
            maxElem = elem
          }
        }
        pq += Tuple2(Tuple2(hashIndex, newHash), priority)
        if (maxElem + 1 < k) {
          var s1 = (e._1 - maxElem) + (maxElem + 1)
          var s2 = e._1 + (maxElem + 1)
          pq2 += Tuple2(s1, priority - this.priorities(this.indices(maxElem)) + this.priorities(this.indices(maxElem + 1)))
          pq2 += Tuple2(s2, priority + this.priorities(this.indices(maxElem + 1)))
        }
      }

      hyperIndex = 0
      hashIndex +=1
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

