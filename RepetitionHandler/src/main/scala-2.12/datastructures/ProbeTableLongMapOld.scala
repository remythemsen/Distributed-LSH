package datastructures

import hashfunctions.HashFunction
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ProbeTableLongMapOld(hashFunction: HashFunction, maxCands:Int) {
  private val table = new mutable.LongMap[ArrayBuffer[Int]]()

  // internal Hash function
  private val hf = hashFunction

  /**
    * Insert vector
    * @param v vector to be inserted into internal hashmap
    */
  def +=(v:((Int, Array[Float]), Int)) : Unit = {
    // add address of vector to the buffer in map
    val key = toLong(hf(v._1._2))
    if(!this.table.contains(key)) {
      this.table(key) = new ArrayBuffer()
    }

    this.table(key) += (v._2)
  }

  /**
    * @param v a query point
    * @return a list of vectors with same key as v
    */
  def query(v:Array[Float]) : ArrayBuffer[Int] = {
    // TODO optimize
    val results = new ArrayBuffer[Int]
    val probes = hf.generateProbes(hf(v))
    var i = 0
    while(results.size < maxCands && i < probes.length) {
      // TODO Is getOrElse fast enough, and we are appending arrays
      this.table.get(toLong(probes(i))) match {
        case Some(x) => results ++= x
        case None =>
      }
      i+=1
    }
    results
  }

  // TODO Move this to the hash function
  def toLong(key:Array[Int]):Long = {
    var i = 0
    var long = 0
    while(i < key.length) {
      long += key(i)
      long = long << 1
      i+=1
    }
    long
  }
}

