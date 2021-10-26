package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _703_KthLargestElementInAStream:
  def main(args: Array[String]): Unit =
    val kthLargest = new KthLargest(2, Array(0))
    println(kthLargest.add(-1))
    println(kthLargest.add(1))
    println(kthLargest.add(-2))
    println(kthLargest.add(-4))
    println(kthLargest.add(3))

  class KthLargest(_k: Int, _nums: Array[Int]):
    given Ordering[Int] = (x, y) => y.compareTo(x)

    val maxK = mutable.PriorityQueue.from(_nums.sorted.take(_k))

    def add(`val`: Int): Int =
      if maxK.size < _k then maxK.enqueue(`val`)
      else if maxK.head <= `val` then
        maxK.dequeue()
        maxK.enqueue(`val`)

      maxK.head
