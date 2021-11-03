package io.github.cosminci.leetcode._100

object _89_GrayCode:
  def main(args: Array[String]): Unit =
    println(grayCode(4))

  def grayCode(n: Int): List[Int] =
    grayCodeBinary(n).map(Integer.parseInt(_, 2)).toList

  def grayCodeBinary(n: Int): Array[String] =
    if n == 1 then return Array("0", "1")

    val prev    = grayCodeBinary(n - 1)
    val prevRev = prev.reverse
    prev.map(_.prepended('0')) ++ prevRev.map(_.prepended('1'))
