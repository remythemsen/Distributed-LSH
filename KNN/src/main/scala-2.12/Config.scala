import java.io._

case class Config(
                   data:File = new File("."),
                   n:Int = 0,
                   dimensions:Int = 0,
                   k:Int = 50,
                   queryPoints:File = new File("."),
                   measure:String = "."
                 )


