package io.github.cosminci.leetcode._200

object _118_PascalsTriangle {
  def main(args: Array[String]): Unit = {
    println(generate(1))
  }

  def generate(numRows: Int): List[List[Int]] = {
    (2 to numRows).scanLeft(Seq(1)) {
      case (prevRow, _) =>
        val newRow = (1 until prevRow.length).map(i => prevRow(i - 1) + prevRow(i))
        newRow.prepended(1).appended(1)
    }.map(_.toList).toList
  }
}
