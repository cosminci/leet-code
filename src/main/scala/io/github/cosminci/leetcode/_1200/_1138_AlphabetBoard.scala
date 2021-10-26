package io.github.cosminci.leetcode._1200

import scala.math.Integral.Implicits._

object _1138_AlphabetBoard {
  def main(args: Array[String]): Unit = {
    println(alphabetBoardPath("leet"))
    println(alphabetBoardPath("code"))
    println(alphabetBoardPath("zdz"))
  }

  private def alphabetBoardPath(target: String): String =
    target.foldLeft("", 0, 0) { case ((path, x1, y1), char) =>
      val (x2, y2) = (char - 'a') /% 5
      val xMove    = if (x1 < x2) "D" * (x2 - x1) else "U" * (x1 - x2)
      val yMove    = if (y1 < y2) "R" * (y2 - y1) else "L" * (y1 - y2)
      val newMove  = if (x2 == 5) s"$yMove$xMove" else s"$xMove$yMove"
      (s"$path$newMove!", x2, y2)
    }._1
}
