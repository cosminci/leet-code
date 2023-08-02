package com.leetcode.cosminci._100

import scala.collection.mutable

object _46_Permutations:

  def permute(nums: Array[Int]): List[List[Int]] =
    val mem = mutable.Map.empty[Set[Int], List[List[Int]]]
    def dfs(nums: Set[Int]): List[List[Int]] = mem.getOrElseUpdate(nums,
      if nums.size == 1 then return List(List(nums.head))
      else nums.flatMap(n => dfs(nums - n).map(_.prepended(n))).toList
    )
    dfs(nums.toSet)
