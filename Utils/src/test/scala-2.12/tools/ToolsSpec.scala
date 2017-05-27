package tools

import org.scalatest.{FlatSpec, Matchers}
import tools.Tools.PQOrd

import scala.collection.mutable
import scala.util.Random

class ToolsSpec extends FlatSpec with Matchers {
  "knn " should "produce correct result!" in {
    val rnd = new Random

    val n = 50
    val lookUpMap = new Array[Int](n)
    for(i <- lookUpMap.indices) {
      lookUpMap.update(i, 100+i)
    }
    val eucData = Array.fill[Array[Float]](50)(Array.fill[Float](128)(rnd.nextFloat))
    var pq = new mutable.PriorityQueue[Int]()(PQOrd)
    var cd = new CandSet(1)

    for(j <- 0 until 1000) {
      cd = new CandSet(1)
      for(i <- 0 until 50) {
        cd +=(i, i.toDouble)
      }
      Tools.knn(cd, eucData, lookUpMap, pq, eucData(rnd.nextInt(eucData.length)),1)
      assert(cd.dists.getDouble(0) == 0.0)
    }

    cd.softReset
    for(i <- 0 until 50) {
      cd +=(i, i.toDouble)
    }
    Tools.knn(cd, eucData, lookUpMap, pq, eucData(rnd.nextInt(eucData.length)),20)
    assert(cd.dists.getDouble(cd.size-1) == 0.0)


  }
}
