package datastructures

import hashfunctions.HashFunctionLong

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ProbeTableLong(hashFunction: HashFunctionLong, maxCands:Int) {
  private val table = new mutable.LongMap[ArrayBuffer[Int]]()

  // internal Hash function
  val hf = hashFunction

  /**
    * Insert vector
    * @param v vector to be inserted into internal hashmap
    */
  def +=(v:((Int, Array[Float]), Int)) : Unit = {
    // add address of vector to the buffer in map
    val key = hf(v._1._2)
    // TODO remove this branch if possible
    if(!this.table.contains(key)) {
      this.table(key) = new ArrayBuffer()
    }

    this.table(key) += v._2
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
    while(results.size < this.maxCands && i < probes.length) {
      this.table.get(probes(i)) match {
        case Some(x) => results ++= x
        case None =>
      }
      i+=1
    }
    results
  }

  def get(key:Long) : ArrayBuffer[Int] = {
    this.table.getOrElse(key, ArrayBuffer())
  }
}

