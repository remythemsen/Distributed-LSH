package datastructures

import hashfunctions.HashFunctionLong

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ProbeTableLong(hashFunction: HashFunctionLong) {
  private val table = new mutable.LongMap[ArrayBuffer[Int]]()

  // internal Hash function
  private val hf = hashFunction

  /**
    * Insert vector
    * @param v vector to be inserted into internal hashmap
    */
  def +=(v:((Int, Array[Float]), Int)) : Unit = {
    // add address of vector to the buffer in map
    val key = hf(v._1._2)
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
    while(i < probes.length) {
      // TODO Is getOrElse fast enough, and we are appending arrays
      results ++= this.table.getOrElse(probes(i), ArrayBuffer.empty)
      i+=1
    }
    results
  }
}

