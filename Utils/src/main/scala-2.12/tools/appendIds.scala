package tools

import java.io.{BufferedWriter, File, FileWriter}

import io.Parser.DisaParserNumeric

import scala.io.Source

/**
  * Created by remeeh on 30-04-2017.
  */
object appendIds extends App {

  val inFile = new File("C:\\datasets\\disa\\0\\descriptors-1-million-reduced-128-hamming-256bit.data")
  val lookUpFile = new File("C:\\datasets\\disa\\0\\descriptors-1-million-reduced-128.data")
  val outFile = new File("C:\\datasets\\disa\\0\\descriptors-1-million-reduced-128-hamming-256bit-ids.data")
  val lUP = DisaParserNumeric(Source.fromFile(lookUpFile).getLines(), 128)

  val p = Source.fromFile(inFile).getLines()
  while(p.hasNext) {
    val t = p.next()
    val id = lUP.next()._1
    writeResult(id + " " + t, outFile)
  }

  def writeResult(line:String, file:File) : Unit = {
    val bw = new BufferedWriter(new FileWriter(file,true))
    bw.append(line+"\n")
    bw.close()
  }
}
