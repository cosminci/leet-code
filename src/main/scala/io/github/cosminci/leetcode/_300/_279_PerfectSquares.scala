package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _279_PerfectSquares:
  def main(args: Array[String]): Unit =
    println(numSquaresTopDown(12))
    println(numSquaresBottomUp(12))

  def numSquaresBottomUp(num: Int): Int =
    val dp = Array.fill[Int](num + 1)(num)
    dp(0) = 0

    (1 to num).foreach { target =>
      (1 to target).takeWhile(v => v * v <= target).foreach { v =>
        val square = v * v
        dp(target) = math.min(dp(target), dp(target - square) + 1)
      }
    }
    dp.last

  def numSquaresTopDown(num: Int): Int =
    val mem = mutable.Map.empty[Int, Int]

    def dfs(n: Int): Int =
      if n == 0 then return 0
      if mem.contains(n) then return mem(n)

      val result = (1 to math.sqrt(n).toInt).map { sqroot =>
        numSquaresTopDown(n - sqroot * sqroot)
      }.min + 1

      mem.update(n, result)
      result

    dfs(num)
