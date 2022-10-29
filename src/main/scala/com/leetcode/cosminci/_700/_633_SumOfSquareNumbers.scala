package com.leetcode.cosminci._700

object _633_SumOfSquareNumbers:
  def main(args: Array[String]): Unit =
    println(judgeSquareSum(3))

  def judgeSquareSum(c: Int): Boolean =
    var (l, r) = (0, math.sqrt(c).toInt)

    while l <= r do
      val curr = l * l + r * r
      if curr < c then l += 1
      else if curr > c then r -= 1
      else return true

    false
