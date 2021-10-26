package io.github.cosminci.leetcode._400

object _357_CountNumbersWithUniqueDigits:
  def main(args: Array[String]): Unit =
    println(countNumbersWithUniqueDigits(1))
    println(countNumbersWithUniqueDigits(2))

  private def countNumbersWithUniqueDigits(n: Int): Int =
    if n == 0 then 1 else (1 to n - 1).map(10 - _).scanLeft(9)(_ * _).sum + 1
