package datastructures

import hashfunctions.HashFunction

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Table[A](hashFunction: HashFunction[A], dSetRef:Array[A]) {
  private val table = new mutable.LongMap[ArrayBuffer[Int]]()
  private val dataSetRef = dSetRef

  // internal Hash function
  private val hf = hashFunction

  /**
    * Insert vector
    * @param v vector reference to be inserted into internal hashmap
    */
  def +=(v:Int) : Unit = {
    // add address of vector to the buffer in map
    val key:Long = hf(dataSetRef(v))
    // TODO remove this branch if possible
    if(!this.table.contains(key)) {
      this.table(key) = new ArrayBuffer()
    }

    this.table(key) += v
  }

  /**
    * @param key a query point hashed key
    * @return a list of vectors with same key as v
    */
  def query(key:Long) : ArrayBuffer[Int] = {
    this.table.getOrElse(key, ArrayBuffer())
  }

  def clear:Unit = {
    this.table.clear
  }

}

