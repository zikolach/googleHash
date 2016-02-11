import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.annotation.tailrec
import scala.io.Source

object GoogleHash {

  def p(i: Int, s: String): List[String] = {
    var j = 0
    var cmds = List.empty[String]
    for (c <- s) {
      val c1 = "" + c
      c1 match {
        case "#" => cmds ::= s"PAINT_LINE $i $j $i $j"
        case _ =>
      }
      j = j + 1
    }
    cmds.reverse
  }

  def p1(i: Int, str: String): List[String] = {
    @tailrec
    def findAll(acc: List[(Int, Int)], from: Int): List[(Int, Int)] = {
      val s = str.indexOf("#", from)
      val e = str.indexOf(".", s)
      (s, e) match {
        case (-1, _) => acc
        case (_, -1) => (s, str.length - s) :: acc
        case _ => findAll((s, e - s) :: acc, e)
      }
    }
    findAll(List.empty, 0).map { case (s, l) =>
      s"PAINT_LINE $i $s $i ${s + l - 1}"
    } reverse
  }

  def main(args: Array[String]): Unit = {
//        val filename = "logo"
        val filename = "right_angle"
//    val filename = "learn_and_teach"
    val tmp = Source.fromFile(Paths.get(s"$filename.in").toFile).getLines().toList

    val data = tmp.head.split(' ')
    val rows = Integer.parseInt(data(0))
    val cols = Integer.parseInt(data(1))

    var i = 0
    val commands = tmp.tail.zipWithIndex.flatMap {
      case (line, index) => p1(index, line)
    }
    commands.foreach(println)

    Files.write(Paths.get(s"$filename.out"), (commands.length.toString :: commands).mkString("\n").getBytes(StandardCharsets.UTF_8))
  }
}