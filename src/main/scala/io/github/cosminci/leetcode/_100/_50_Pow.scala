package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _50_Pow:
  def main(args: Array[String]): Unit =
    println(myPow(2, Int.MinValue))
    println(myPow(-2.0, -2))

  private def myPow(x: Double, n: Int): Double =
    def dfs(num: Double, pow: Int): Double =
      if pow == 0 then return 1
      val oddFactor = if pow % 2 == 0 then 1 else num
      dfs(num * num, pow / 2) * oddFactor

    if n < 0 then 1 / dfs(x, math.abs(n)) else dfs(x, n)
