package datastructures

import hashfunctions.HashFunction
import it.unimi.dsi.fastutil.ints.IntArrayList

import scala.collection.mutable

class Table[A](hashFunction: HashFunction[A], dSetRef:Array[A]) {
  private val table = new mutable.LongMap[IntArrayList]()// new mutable.LongMap[ArrayBuffer[Int]]()
  private val dataSetRef = dSetRef

  // internal Hash function
  private val hf = hashFunction

  /**
    * Insert vector
    * @param v vector reference to be inserted into internal hashmap
    */
  def +=(v:Int) : Unit = {
    // add address of vector to the buffer in map
    val key = hf(dataSetRef(v))
    // TODO remove this branch if possible
    if(!this.table.contains(key)) {
      this.table.put(key, new IntArrayList())
    }
    this.table(key).add(v)
  }

  /**
    * @param key a query point hashed key
    * @return a list of vectors with same key as v
    */
  def query(key:Long) : IntArrayList = {
    this.table.getOrNull(key)
  }

  def clear:Unit = {
    this.table.clear
  }

}

