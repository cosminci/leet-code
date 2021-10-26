package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _416_PartitionEqualSubsetSum:
  def main(args: Array[String]): Unit =
    println(canPartitionBottomUp(Array(1, 2, 5)))
    println(canPartitionTopDown(Array(1, 2, 5)))

  private def canPartitionBottomUp(nums: Array[Int]): Boolean =
    val total = nums.sum
    if total % 2 == 1 || nums.length < 2 then return false
    val target = total / 2

    val prevSums = mutable.Set(0)
    (nums.length - 1 to 0 by -1).foreach { idx =>
      val newSums = prevSums.map(_ + nums(idx))
      if newSums.contains(target) then return true
      prevSums.addAll(newSums)
    }
    false

  private def canPartitionTopDown(nums: Array[Int]): Boolean =
    val total = nums.sum
    if total % 2 == 1 || nums.length < 2 then return false
    val target = total / 2

    val mem = mutable.Map.empty[(Int, Int), Boolean]
    def dfs(idx: Int, target: Int): Boolean =
      if idx == nums.length then return target == 0
      if mem.contains((idx, target)) then return mem((idx, target))

      val result = dfs(idx + 1, target - nums(idx)) || dfs(idx + 1, target)
      mem.update((idx, target), result)
      result

    dfs(0, target)
