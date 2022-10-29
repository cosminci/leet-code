package com.leetcode.cosminci._2400

import scala.collection.mutable

object _2386_FindKSumOfArray:

  def kSum(nums: Array[Int], k: Int): Long =
    val maxSum = nums.map(_.toLong).filter(_ > 0).sum
    if k == 1 then return maxSum

    val absNums = nums.map(_.abs).sorted
    val highest = mutable.PriorityQueue((maxSum - absNums.head, 0))

    (0 until k - 2).foreach { _ =>
      val (nextSum, i) = highest.dequeue()
      if absNums.isDefinedAt(i + 1) then
        highest.enqueue((nextSum + absNums(i) - absNums(i + 1), i + 1))
        highest.enqueue((nextSum - absNums(i + 1), i + 1))
    }

    highest.dequeue()._1
