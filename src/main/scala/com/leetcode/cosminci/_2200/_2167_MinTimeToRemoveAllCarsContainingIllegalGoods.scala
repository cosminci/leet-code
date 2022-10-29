package com.leetcode.cosminci._2200

import scala.collection.mutable

object _2167_MinTimeToRemoveAllCarsContainingIllegalGoods:
  def minimumTime(s: String): Int =
    val nums = s.indices.map(i => if s(i) == '1' then 1 else -1)
    val mem  = mutable.Map.empty[Int, Int]

    def dfs(i: Int): Int = mem.getOrElseUpdate(i, {
      if i == 0 then nums.head
      else nums(i).min(nums(i) + dfs(i - 1))
    })

    s.length + s.indices.map(dfs).min
