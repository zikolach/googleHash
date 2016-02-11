import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

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

  def findLines(i: Int, str: String): List[(Int, Int, Int, Int)] = {
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
      (i, s, i, s + l - 1)
    }
  }

  def p1(i: Int, str: String): List[String] = {
    findLines(i, str).map { case (r1, c1, r2, c2) =>
      s"PAINT_LINE $r1 $c1 $r2 $c2"
    } reverse
  }

  def processLines1(lines: List[String]): List[String] = {
    lines.tail.zipWithIndex.flatMap {
      case (line, index) => p1(index, line)
    }
  }

  def processLines2(lines: List[String]): List[String] = {
    val h = lines.tail.zipWithIndex.flatMap {
      case (line, index) => findLines(index, line)
    }
    val v = lines.tail.transpose.zipWithIndex.flatMap {
      case (line, index) => findLines(index, line.mkString)
    }
    val tuples = if (v.length < h.length) {
      v.map(t => (t._2, t._1, t._4, t._3))
    } else {
      h
    }
    tuples map { case (r1, c1, r2, c2) =>
      s"PAINT_LINE $r1 $c1 $r2 $c2"
    }
  }

  def main(args: Array[String]): Unit = {
    // val filename = "logo"
    // val filename = "right_angle"
    val filename = "learn_and_teach"
    val tmp = Source.fromFile(Paths.get(s"$filename.in").toFile).getLines().toList

    val commands = processLines2(tmp)
    commands.foreach(println)

    Files.write(Paths.get(s"$filename.out"), (commands.length.toString :: commands).mkString("\n").getBytes(StandardCharsets.UTF_8))
  }
}