package com.leetcode.cosminci._2900

import scala.collection.mutable

object _2826_SortingThreeGroups:

  def minimumOperations(nums: List[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, prevGroup: Int): Int = mem.getOrElseUpdate((i, prevGroup),
      if i == nums.length then 0
      else (prevGroup to 3)
        .map(newGroup => if nums(i) != newGroup then dfs(i + 1, newGroup) + 1 else dfs(i + 1, newGroup))
        .minOption.getOrElse(Int.MaxValue)
    )

    dfs(i = 0, prevGroup = 1)
