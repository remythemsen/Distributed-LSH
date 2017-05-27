import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.util.concurrent.{ArrayBlockingQueue, Executors}
import io.Parser.DisaParserRaw
import scopt.OptionParser
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.Random


// Args config
case class Config(
                   data:File = new File("."),
                   n:Int = 0,
                   outDir:String = ".",
                   dataDim:Int = 4096,
                   targetDim:Int = 128,
                   binary:Boolean = false,
                   randomSeed:Long = System.currentTimeMillis()
                 )

object Program extends App {
  val parser = new OptionParser[Config]("Reducer") {
    head("Reducer", "1.0")

    opt[File]('d', "data").required().valueName("<file>").action((x, c) =>
      c.copy(data = x)).text("input data file!")

    opt[Int]('n', "size").required().valueName("<int>").action((x, c) =>
      c.copy(n = x)).text("input data file size")

    opt[String]('o', "out").valueName("<string>").required().action((x, c) =>
      c.copy(outDir = x)).text("out file directory (without trailing slash")

    opt[Int]('d', "dataDimension").valueName("<int>").required().action((x, c) =>
      c.copy(dataDim = x)).text("input vector dimensions, default 4096")

    opt[Int]('t', "targetDimension").valueName("<int>").required().action((x, c) =>
      c.copy(targetDim = x)).text("target vector dimensions, default 256")

    opt[Boolean]('b', "binary").valueName("<false|true>").required().action((x, c) =>
      c.copy(binary = x)).text("should out file contain binary values, default false")

    opt[Long]('r', "seed").valueName("<long>").action((x, c) =>
      c.copy(randomSeed = x)).text("random seed for matrix generation, default system time")

    help("help").text("prints this usage text")
  }

  parser.parse(args, Config()) match {
    case Some(config) => {

      implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(8))

      val dimensions = config.targetDim
      val sqrtTargetDim = Math.sqrt(dimensions.toDouble)

      // TODO Find replacement of random
      val rnd = new Random(config.randomSeed)


      val p = 6
      // Number of threads // Same time no matter what number from 4-16
      val loadedTuples = new ArrayBlockingQueue[(Int, Array[Float])](10000)
      val preProcessedTuples = new ArrayBlockingQueue[(Int, Array[Float])](20)

      println("Loading files...")
      val input = DisaParserRaw(Source.fromFile(config.data).getLines, config.dataDim)
      val originalDimensions = config.dataDim
      val n = config.n

      println("Generating Random Matrix...")

      val randomMatrix = DimensionalityReducer.getRandMatrix(dimensions, originalDimensions, rnd.nextLong)

      val binary = config.binary
      var progress = 0

      println("Starting reduction...")

      Future {
        while (input.hasNext) {
          loadedTuples.put(input.next)

        }
      }

      for (i <- 0 until p) {
        Future {
          while (true) {
            var tuple = loadedTuples.take()
            require(tuple._2.length == originalDimensions)
            val aux = new Array[Float](dimensions)
            DimensionalityReducer.getNewVector(tuple._2, randomMatrix, aux, binary, rnd.nextLong, sqrtTargetDim)
            val reducedTuple = (tuple._1, aux)
            preProcessedTuples.put(reducedTuple)
          }
        }
      }

      val isBinary = {
        if (binary)
          "-" + config.binary
        else ""
      }
      val dir: String = config.outDir.concat("/")
        // constructing filename
        .concat(config.data.getName.substring(0, config.data.getName.length - 5))
        .concat("-reduced-" + dimensions + isBinary + ".data")

      val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dir.toString)))


      var j = 0.0
      val percentile = n / 100

      while (progress != n) {

        var t = preProcessedTuples.take()
        require(t._2.length == dimensions)

        var sb = new StringBuffer(456)
        sb.append(t._1)
        sb.append(" ")
        for (component <- t._2) {
          if (binary)
            sb.append(component.toInt + " ")
          else
            sb.append(component + " ")
        }
        sb.append("\n")

        // Write resulting set
        output.write(sb.toString)


        j += 1.0
        progress += 1
        if (j % percentile == 0) {
          println(((j / n) * 100).toInt + "%")
        }
      }
      output.close()
      println("\nFinished with " + progress + " out of " + n + " tuples...")
      print("Checking dataset...")
      val newFile = Source.fromFile(new File(dir)).getLines()
      require(newFile.length == n)
      print("Ok!\n")

    }
  }
}

