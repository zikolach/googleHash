import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.io.Source

object GoogleHash {

  def p(i: Int, s: String): List[String] = {
    var j = 0
    var cmds = List.empty[String]
    for (c <- s) {
      val c1 = "" + c
      c1 match {
        case "#" =>
          //          Console.println("PAINT_LINE " + i + " " + j + " " + i + " " + j)
          cmds ::= s"PAINT_LINE $i $j $i $j"
          co = co + 1
        case _ =>
      }
      j = j + 1
    }
    cmds.reverse
  }

  var co = 0

  def main(args: Array[String]): Unit = {
    //    val filename = "logo"
    //    val filename = "right_angle"
    val filename = "learn_and_teach"
    val tmp = Source.fromFile(Paths.get(s"$filename.in").toFile).getLines().toList

    val data = tmp.head.split(' ')
    val rows = Integer.parseInt(data(0))
    val cols = Integer.parseInt(data(1))

    var i = 0
    val commands = tmp.tail.zipWithIndex.flatMap {
      case (line, index) => p(index, line)
    }
    commands.foreach(println)

    Files.write(Paths.get(s"$filename.out"), (commands.length.toString :: commands).mkString("\n").getBytes(StandardCharsets.UTF_8))
  }
}