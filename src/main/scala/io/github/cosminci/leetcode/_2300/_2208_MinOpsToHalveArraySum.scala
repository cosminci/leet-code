package io.github.cosminci.leetcode._2300

import scala.collection.mutable

object _2208_MinOpsToHalveArraySum:
  def halveArray(nums: Array[Int]): Int =
    val pqueue = mutable.PriorityQueue.from(nums.map(_.toDouble))
    val target = nums.map(_.toDouble).sum / 2

    @annotation.tailrec
    def dfs(removed: Double, step: Int): Int =
      if removed >= target then step
      else
        val highest = pqueue.dequeue()
        pqueue.enqueue(highest / 2)
        dfs(removed + highest / 2, step + 1)

    dfs(removed = 0, step = 0)
