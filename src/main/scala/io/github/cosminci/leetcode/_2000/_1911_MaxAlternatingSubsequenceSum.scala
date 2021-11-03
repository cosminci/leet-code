package io.github.cosminci.leetcode._2000

import scala.collection.mutable

object _1911_MaxAlternatingSubsequenceSum:
  def main(args: Array[String]): Unit =
    val input = Array(374, 126, 84, 237, 195, 139, 328, 353)
    println(maxAlternatingSumTopDown(input))
    println(maxAlternatingSumBottomUp(input))

  def maxAlternatingSumTopDown(nums: Array[Int]): Long =
    val mem = mutable.Map.empty[(Int, Boolean), Long]
    def dfs(idx: Int, prevAdded: Boolean): Long =
      if idx == nums.length then return 0
      if mem.contains((idx, prevAdded)) then return mem((idx, prevAdded))

      val delta = if prevAdded then -nums(idx) else nums(idx)
      val max   = math.max(dfs(idx + 1, prevAdded = !prevAdded) + delta, dfs(idx + 1, prevAdded))
      mem.update((idx, prevAdded), max)
      max
    dfs(0, prevAdded = false)

  def maxAlternatingSumBottomUp(nums: Array[Int]): Long =
    var prevEven = 0L
    var prevOdd  = 0L
    nums.indices.foreach { i =>
      val newEven = math.max(prevEven, prevOdd + nums(i))
      prevOdd = math.max(prevOdd, prevEven - nums(i))
      prevEven = newEven
    }
    prevEven
