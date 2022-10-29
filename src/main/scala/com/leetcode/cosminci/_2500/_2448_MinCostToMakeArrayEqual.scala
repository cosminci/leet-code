package com.leetcode.cosminci._2500

import scala.util.chaining.*

object _2448_MinCostToMakeArrayEqual:

  def minCost(nums: Array[Int], cost: Array[Int]): Long =
    val totalCost = cost.foldLeft(0L)(_ + _)
    val sorted    = nums.zip(cost).sorted

    @annotation.tailrec
    def dfs(i: Int, sum: Long): Int =
      val (num, cost) = sorted(i)
      if sum + cost > totalCost / 2 then num else dfs(i + 1, sum + cost)

    val target = dfs(i = 0, sum = 0L)
    sorted.map { case (num, cost) => cost.toLong * (num - target).abs }.sum
