package com.leetcode.cosminci._2800

import scala.collection.mutable

object _2741_SpecialPermutations:

  def specialPerm(nums: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, mask: Int): Int = mem.getOrElseUpdate((i, mask),
      if mask == (1 << nums.length) - 1 then 1
      else nums.indices.foldLeft(0L)((acc, j) =>
        if (mask & (1 << j)) != 0 || (mask != 0 && nums(i) % nums(j) != 0 && nums(j) % nums(i) != 0) then acc
        else (acc + dfs(j, mask | (1 << j))) % 1_000_000_007
      ).toInt
    )
    dfs(i = 0, mask = 0)
