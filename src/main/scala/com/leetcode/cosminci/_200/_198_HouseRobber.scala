package com.leetcode.cosminci._200

import scala.collection.mutable

object _198_HouseRobber:
  def main(args: Array[String]): Unit =
    println(robTopDown(Array(2, 1, 1, 2, 5, 6, 7, 8, 2, 3, 5)))
    println(robBottomUp(Array(2, 1, 1, 2, 5, 6, 7, 8, 2, 3, 5)))

  def robTopDown(nums: Array[Int]): Int =
    val mem = mutable.Map.empty[Int, Int]
    def dfs(idx: Int): Int =
      mem.getOrElseUpdate(idx, {
        if idx >= nums.length then 0
        else (nums(idx) + dfs(idx + 2)).max(dfs(idx + 1))
      })
    dfs(idx = 0)

  def robBottomUp(nums: Array[Int]): Int =
    if nums.length == 1 then return nums.head
    if nums.length == 2 then return math.max(nums(0), nums(1))
    var prev2 = nums(0)
    var prev1 = math.max(nums(0), nums(1))

    (2 until nums.length).foreach { idx =>
      val maxLocal = math.max(nums(idx) + prev2, prev1)
      prev2 = prev1
      prev1 = maxLocal
    }
    prev1
