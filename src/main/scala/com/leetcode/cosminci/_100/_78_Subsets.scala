package com.leetcode.cosminci._100

import scala.collection.mutable

object _78_Subsets:
  def subsets(nums: Array[Int]): List[List[Int]] =
    val mem = mutable.Map.empty[Int, List[List[Int]]]

    def dfs(idx: Int): List[List[Int]] =
      mem.getOrElseUpdate(idx, {
        if idx == nums.length then List(List.empty)
        else dfs(idx + 1) ++ dfs(idx + 1).map(_.prepended(nums(idx)))
      })

    dfs(idx = 0)
