package com.leetcode.cosminci._800

import scala.collection.mutable

object _703_KthLargestElementInAStream:
  def main(args: Array[String]): Unit =
    val kthLargest = new KthLargest(2, Array(0))
    println(kthLargest.add(-1))
    println(kthLargest.add(1))
    println(kthLargest.add(-2))
    println(kthLargest.add(-4))
    println(kthLargest.add(3))

  class KthLargest(k: Int, nums: Array[Int]):
    given Ordering[Int] = (x, y) => y.compareTo(x)
    private val maxK    = mutable.PriorityQueue.from(nums.sorted.take(k))

    def add(value: Int): Int =
      maxK.enqueue(value)
      if maxK.size > k then maxK.dequeue()
      maxK.head
