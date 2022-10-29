package com.leetcode.cosminci._2100

import scala.collection.mutable

object _2044_CountNumberOfMaxBitwiseORSubsets:
  def main(args: Array[String]): Unit =
    println(countMaxOrSubsets(Array(3, 2, 1, 5)))

  def countMaxOrSubsets(nums: Array[Int]): Int =
    val max = nums.reduce(_ | _)
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(idx: Int, mask: Int): Int =
      mem.getOrElseUpdate((idx, mask), {
        if mask == max then math.pow(2, nums.length - idx).toInt
        else if idx == nums.length then 0
        else dfs(idx + 1, mask | nums(idx)) + dfs(idx + 1, mask)
      })

    dfs(idx = 0, mask = 0)
