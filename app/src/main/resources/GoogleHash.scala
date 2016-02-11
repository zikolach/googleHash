import scala.io.Source

/**
  * <pre> </pre>
  */


object GoogleHash {


  def process(l: String) = {

  }

  def p(i: Int, s: String) = {
    val l = s.length
    var j = 0
    for (c <- s) {
      val c1 = "" + c

      c1 match {
        case "#" => {
          Console.println("PAINT_LINE " + i + " " + j +" "+ i +" "+ j)
          co = co + 1
        }
        case _ =>
      }
      j = j + 1
    }
  }
var co = 0
  def main(args: Array[String]): Unit = {
    val tmp: Iterator[String] = Source.fromURL(getClass.getResource("/logo.in")).getLines
//    val tmp: Iterator[String] = Source.fromURL(getClass.getResource("/right_angle.in")).getLines
//    val tmp: Iterator[String] = Source.fromURL(getClass.getResource("/learn_and_teach.in")).getLines


    val data = tmp.next().split(' ')
    val rows = Integer.parseInt(data(0))
    val cols = Integer.parseInt(data(1))

    var i = 0
    while (i < rows) {
      p(i, tmp.next())
      i = i + 1
    }
    println(co)
  }
}