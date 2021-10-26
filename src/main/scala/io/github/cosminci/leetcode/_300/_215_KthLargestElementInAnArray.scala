package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _215_KthLargestElementInAnArray:
  def main(args: Array[String]): Unit =
    println(findKthLargest(Array(3, 2, 1, 5, 6, 4), 2))
    println(findKthLargestPivot(Array(3, 2, 1, 5, 6, 4), 2))
    println(findKthLargestPivot(Array(4, 2, 3, 1, 2, 3, 5, 5, 6), 9))

  def findKthLargest(nums: Array[Int], k: Int): Int =
    val smallest = mutable.PriorityQueue.from(nums)
    var result   = 0
    (1 to k).foreach(_ => result = smallest.dequeue())
    result

  def findKthLargestPivot(nums: Array[Int], k: Int): Int =
    val target = nums.length - k

    def swap(i: Int, j: Int) =
      val tmp = nums(j)
      nums(j) = nums(i)
      nums(i) = tmp

    def findPivot(start: Int, end: Int): Int =
      val pivot = nums(end)
      var i     = start
      (start to end).foreach { j =>
        if nums(j) <= pivot then
          swap(i, j)
          i += 1
      }
      if i - 1 == target then pivot
      else if i - 1 < target then findPivot(i, end)
      else findPivot(start, i - 2)

    findPivot(0, nums.length - 1)
