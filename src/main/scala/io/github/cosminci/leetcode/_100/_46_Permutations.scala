package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _46_Permutations:
  def main(args: Array[String]): Unit =
    println(permute(Array(1, 2, 3)))

  def permute(nums: Array[Int]): List[List[Int]] =
    val mem = mutable.Map.empty[Set[Int], List[List[Int]]]
    
    def dfs(nums: Set[Int]): List[List[Int]] =
      if nums.size == 1 then return List(List(nums.head))
      if mem.contains(nums) then return mem(nums)

      val result = nums.flatMap(n => dfs(nums - n).map(_.prepended(n))).toList
      mem.update(nums, result)
      result
      
    dfs(nums.toSet)
