package tools

import java.io._

import scala.collection.mutable

/**
  * Created by remeeh on 11-04-2017.
  */
object Program extends App {
  for(i <- 0 until 10) {
    val w = new writeMapToFile(loadKNNStructure(new File("C:/datasets/distributed_lsh/"+i+"/structure-Euclidean")))
    w.write("C:/datasets/distributed_lsh/"+i+"/knnSets.txt")
  }

  def loadKNNStructure(file:File): mutable.HashMap[Int, Array[(Int, Double)]] = {
    println("Loading Optimal KNN structure")
    val objReader = new ObjectInputStream(new FileInputStream(file))
    val hashMap = objReader.readObject.asInstanceOf[mutable.HashMap[Int, Array[(Int, Double)]]]
    objReader.close()
    hashMap
  }
}

class writeMapToFile(map:mutable.HashMap[Int, Array[(Int, Double)]]) {

  def write(file:String) : Unit = {
    val arr:Array[(Int, Array[(Int, Double)])] = map.toArray
    for(i <- 0 until arr.length) {
      val res:String = {
        arr(i)._1.toString + {
          val sb = new StringBuilder
          for(x <- arr(i)._2) {
            sb.append("," + x._1.toString + " " + x._2.toString)
          }
          sb.toString
        }
      }
      writeResult(res)
    }


    def writeResult(line:String) : Unit = {
      val bw = new BufferedWriter(new FileWriter(file,true))
      bw.append(line+"\n")
      bw.close()
    }
  }
}
