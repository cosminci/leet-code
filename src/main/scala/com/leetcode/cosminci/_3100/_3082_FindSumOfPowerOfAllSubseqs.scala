package com.leetcode.cosminci._3100

import com.leetcode.cosminci.utils.powMod
import scala.collection.mutable

object _3082_FindSumOfPowerOfAllSubseqs:

  def sumOfPower(nums: Array[Int], k: Int): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, sum: Int): Int = mem.getOrElseUpdate((i, sum),
      if sum == k then powMod(2, nums.length - i, mod = 1_000_000_007)
      else if sum > k || i == nums.length then 0
      else ((2L * dfs(i + 1, sum) + dfs(i + 1, sum + nums(i))) % 1_000_000_007).toInt
    )
    dfs(i = 0, sum = 0)
