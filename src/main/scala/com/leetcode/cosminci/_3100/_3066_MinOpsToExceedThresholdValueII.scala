package com.leetcode.cosminci._3100

import scala.collection.mutable

object _3066_MinOpsToExceedThresholdValueII:

  def minOperations(nums: Array[Int], k: Int): Int =
    val pqueue = mutable.PriorityQueue.from(nums.map(_.toLong))(Ordering[Long].reverse)

    @annotation.tailrec
    def dfs(i: Int): Int =
      if pqueue.head >= k then i
      else
        val (x, y) = (pqueue.dequeue(), pqueue.dequeue())
        pqueue.enqueue(x.min(y) * 2 + x.max(y))
        dfs(i + 1)

    dfs(i = 0)
