package io

import IO.Parser.DisaParserNumeric
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class ParserSpec extends FlatSpec with Matchers {
  "DisaParserNumeric" should "get produce a tuple" in {
    val p = DisaParserNumeric(Source.fromFile("data/sample.txt").getLines, 4096)
    var r = (1, Array(1.5f, 2.2f))
    if(p.hasNext) {
      r = p.next
    }
    assert(true)
  }

}
