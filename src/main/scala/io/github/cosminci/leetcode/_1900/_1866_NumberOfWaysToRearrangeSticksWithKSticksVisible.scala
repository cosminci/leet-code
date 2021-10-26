package io.github.cosminci.leetcode._1900

import scala.collection.mutable

object _1866_NumberOfWaysToRearrangeSticksWithKSticksVisible:

  def main(args: Array[String]): Unit =
    println(rearrangeSticks(3, 2))

  private def rearrangeSticks(n: Int, k: Int): Int =
    val mem = mutable.Map.empty[(Int, Int), BigInt]
    def dfs(n: Int, k: Int): BigInt =
      if k == 0 || n == 0 || k > n then return BigInt(0)
      if n == k then return BigInt(1)
      if mem.contains(n, k) then return mem((n, k))

      val result = dfs(n - 1, k - 1) + (n - 1) * dfs(n - 1, k)
      mem.update((n, k), result)
      result
    (dfs(n, k) % (math.pow(10, 9) + 7).toInt).toInt
