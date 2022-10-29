package com.leetcode.cosminci._500

import scala.collection.mutable

object _416_PartitionEqualSubsetSum:
  def main(args: Array[String]): Unit =
    println(canPartitionBottomUp(Array(1, 2, 5)))
    println(canPartitionTopDown(Array(1, 2, 5)))

  def canPartitionBottomUp(nums: Array[Int]): Boolean =
    val total = nums.sum
    if total % 2 == 1 || nums.length < 2 then return false
    val target = total / 2

    val prevSums = mutable.Set(0)
    (nums.length - 1 to 0 by -1).exists { idx =>
      val newSums   = prevSums.map(_ + nums(idx))
      val predicate = newSums.contains(target)
      prevSums.addAll(newSums)
      predicate
    }

  def canPartitionTopDown(nums: Array[Int]): Boolean =
    val total = nums.sum
    if total % 2 == 1 || nums.length < 2 then return false
    val target = total / 2

    val mem = mutable.Map.empty[(Int, Int), Boolean]
    def dfs(idx: Int, target: Int): Boolean =
      mem.getOrElseUpdate((idx, target), {
        if idx == nums.length then target == 0
        else dfs(idx + 1, target - nums(idx)) || dfs(idx + 1, target)
      })

    dfs(0, target)
