package datastructures

import java.util
import hashfunctions.{CrossPolytope, HashFunction}
import multiprobing.{CrossPolytopeScheme, HyperplaneScheme}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ProbeTable(f:() => HashFunction) {
  private val table = new mutable.HashMap[Int, ArrayBuffer[(Int, Array[Float])]]()

  // internal Hash function
  private val hf = f()

  /**
    * Insert vector
    * @param v vector to be inserted into internal hashmap
    */
  def +=(v:(Int, Array[Float])) : Unit = {
    val key = util.Arrays.hashCode(hf(v._2))
    val value = {
      if(this.table.contains(key)) this.table(key)++ArrayBuffer(v)
      else ArrayBuffer(v)
    }
    this.table += (key -> value)
  }

  /**
    * @param v a query point
    * @return a list of vectors with same key as v
    */
  def query(v:Array[Float]) : ArrayBuffer[(Int, Array[Float])] = {
    // TODO dont use Array.hashCode
    for {
      p <- hf.generateProbes(v)
      cands <- this.table(util.Arrays.hashCode(p))
    } yield cands
  }



  def mpQuery(q:Array[Float], probingScheme:String, numOfProbes:Int) : ArrayBuffer[(Int, Array[Float])] = {
    // keys of buckets to be probed
    var bucketsToBeProbed = new ArrayBuffer[Int]

    // TODO Move this out, it needs to be put on the table on launch
    probingScheme match {
      case "Hyperplane" =>
        val p = new HyperplaneScheme(hf(q))
        bucketsToBeProbed = p.generateProbes.map(x => util.Arrays.hashCode(x))

      case "Crosspolytope" =>
        // T = 3
        val rotations = hf.asInstanceOf[CrossPolytope].rotations
        val arrayOfMaxIndices = hf.asInstanceOf[CrossPolytope].arrayOfMaxIndices
        val p = new CrossPolytopeScheme(rotations, arrayOfMaxIndices, numOfProbes)
        bucketsToBeProbed = p.generateProbes.map(x => util.Arrays.hashCode(x))

      case "None" => bucketsToBeProbed = ArrayBuffer(util.Arrays.hashCode(hf(q)))
      case _ => throw new Exception("Unknown Probing scheme")

    }

    var candidates = new ArrayBuffer[(Int, Array[Float])]
    for (b <- bucketsToBeProbed) {
      if(this.table.contains(b)) {
        candidates = candidates++this.table(b)
      }
    }

    candidates
  }

}

