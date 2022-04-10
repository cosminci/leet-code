package io.github.cosminci.leetcode._2300

import scala.collection.mutable

object _2233_MaxProductAfterKIncrements:
  def maximumProduct(nums: Array[Int], k: Int): Int =
    val minHeap = mutable.PriorityQueue.newBuilder(Ordering.Int.reverse).addAll(nums).result()

    @annotation.tailrec
    def dfs(k: Int): Int =
      if k == 0 then minHeap.foldLeft(1L)(_ * _ % 1_000_000_007).toInt
      else
        minHeap.enqueue(minHeap.dequeue() + 1)
        dfs(k - 1)

    dfs(k)
