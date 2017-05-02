package datastructures

import hashfunctions.HashFunction

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Table[A](hashFunction: HashFunction[A]) {
  private val table = new mutable.LongMap[ArrayBuffer[Int]]()

  // internal Hash function
  private val hf = hashFunction

  /**
    * Insert vector
    * @param v vector reference to be inserted into internal hashmap
    */
  def +=(v:((Int, A), Int)) : Unit = {
    // add address of vector to the buffer in map
    val key:Long = hf(v._1._2)
    // TODO remove this branch if possible
    if(!this.table.contains(key)) {
      this.table(key) = new ArrayBuffer()
    }

    this.table(key) += v._2
  }

  /**
    * @param key a query point hashed key
    * @return a list of vectors with same key as v
    */
  def query(key:Long) : ArrayBuffer[Int] = {
    this.table.getOrElse(key, ArrayBuffer())
  }

}

