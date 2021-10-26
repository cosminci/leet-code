package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _1_TwoSum:
  
  def twoSum(nums: Array[Int], target: Int): Array[Int] =
    val seen = mutable.Map.empty[Int, Int]
    nums.zipWithIndex.foreach { (n, idx) =>
      if seen.contains(target - n) then return Array(seen(target - n), idx)
      seen.update(n, idx)
    }
    Array.empty
