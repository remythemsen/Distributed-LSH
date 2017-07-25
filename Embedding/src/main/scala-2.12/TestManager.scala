import java.io.File

import io.Parser.DisaParserNumeric
import measures.{Cosine, Euclidean, Hamming}

import scala.io.Source
import scala.util.Try

object TestManager {
  def loadTestCases(lines: Iterator[String]): List[Either[String,TestCase[Array[Float]]]] = {
    //@annotation.tailrec
    def go(s:Iterator[String], res:List[Either[String, TestCase[Array[Float]]]]) : List[Either[String, TestCase[Array[Float]]]] = {
      s.map(line => {
        var a = line.split(" ")
        Right(TestCase[Array[Float]](
          Try(a(5).toLowerCase).toEither match {
            case Right("euclidean") => Right(Euclidean)
            case Right("cosine") => Right(Cosine)
            case _ => Left(new Exception("Unknown Measure or format!"))
          }, // measure
          // IO Fail detect, int conversion
          Try(DisaParserNumeric(Source.fromFile(a(0)).getLines(),a(9).toInt)).toEither,
          Try(DisaParserNumeric(Source.fromFile(a(1)).getLines(),a(9).toInt)).toEither,
          Try(a(4).toInt).toEither,
          Try(a(7).toInt).toEither, // data size
          Try(a(8).toInt).toEither, // query size
          Try(a(9).toInt).toEither // reduced dimensions
        ))

      })
      ???
    }
    go(lines, List())
  }
}
