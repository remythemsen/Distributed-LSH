import java.io.File

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by remeeh on 5/9/17.
  */
class TesterSpec extends FlatSpec with Matchers {
  "test " should " return correct result for binarydistributed" in {
    val data = "/home/remeeh/IdeaProjects/Distributed-LSH/data/ps-256-tiny.data"
    val dataeuc = "/home/remeeh/IdeaProjects/Distributed-LSH/data/profiset-128-float-tiny.data"
    val dataSize = 500000
    val dimensions = 256
    val seed = 12094
    val nodes = new File("/home/remeeh/IdeaProjects/Distributed-LSH/data/ips")


    val tester = new BinaryDistributed(data, dataeuc, dataSize, dimensions, seed, nodes)
    val tc = new TestCase(
      "/home/remeeh/IdeaProjects/Distributed-LSH/data/ps-256-tiny.query",
      "/home/remeeh/IdeaProjects/Distributed-LSH/data/profiset-128-float.query",
      1,
      12,
      "twostep",
      40000,
      5000,
      "hamming",
      50,
      "/home/remeeh/IdeaProjects/Distributed-LSH/data/profiset-128-float-tiny-euclidean.knn"

    )
    tester.run(tc,1,1)
  }
}
