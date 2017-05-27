package multiprobing

import java.util

import hashfunctions.HashFunction

class TwoStep[A](k:Int, hfs:Array[HashFunction[A]]) extends ProbeScheme[A] {
  // Set of resulting probes for all keys
  var probes:Array[(Int, Long)] = new Array(hfs.length*(1+(k*(k+1)/2)))
  var probesSpent:Int = _
  var hashFunctions:Array[HashFunction[A]] = hfs
  var keys:Array[(Int, Long)] = new Array(hfs.length)

  // When having multiple hashfunctions the ordering should be keys first, then keys with 1 bit changed, then 2 bits
  // We are using three counters to achieve inserting in probes array with this ordering
  // Note: All 1step of hf(1) will preceed all 1step of hf(2) TODO: Make different index system that addresses this
  override def generate(qp:A): Unit = {
    var keysC = 0
    var oneStepC = hfs.length
    var twoStepC = oneStepC+(k*hfs.length)

    // Get keys of qp
    var g = 0
    while(g < this.hfs.length) {
      this.keys(g) = (g,this.hashFunctions(g)(qp))
      g += 1
    }

    var kIndex = 0
    while (kIndex < hfs.length) {
      var i, j = 0
      var oneStepProbe: Long = 0

      // Adding the key itself
      val key = (kIndex, keys(kIndex)._2)
      probes(keysC) = key
      keysC += 1

      while (i < k) {
        probes(oneStepC) = (kIndex, checkAndFlip(key._2, i))
        oneStepProbe = probes(oneStepC)._2

        oneStepC += 1
        j = i + 1
        while (j < k) {
          probes(twoStepC) = (kIndex, checkAndFlip(oneStepProbe, j))
          twoStepC+=1
          j = j + 1
        }
        i += 1
      }

      kIndex += 1
    }

    this.probesSpent = 0

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

  override def hasNext():Boolean = probesSpent < probes.length

  override def next(): (Int, Long) = {
    val r = probes(probesSpent)
    this.probesSpent += 1
    r
  }
}
