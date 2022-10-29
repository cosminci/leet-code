package com.leetcode.cosminci._2100

object _2022_Convert1DArrayTo2DArray {
  def main(args: Array[String]): Unit = {
    println(construct2DArray(Array(1, 2), 1, 1).map(_.toSeq).toSeq)
  }

  def construct2DArray(original: Array[Int], m: Int, n: Int): Array[Array[Int]] =
    if (m * n != original.length) Array.empty
    else Array.tabulate(m, n)((x, y) => original(x * n + y))
}
