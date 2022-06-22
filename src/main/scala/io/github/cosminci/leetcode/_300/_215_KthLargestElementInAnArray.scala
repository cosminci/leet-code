package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _215_KthLargestElementInAnArray:

  def findKthLargest(nums: Array[Int], k: Int): Int =
    def swap(i: Int, j: Int): Unit =
      val tmp = nums(j)
      nums(j) = nums(i)
      nums(i) = tmp

    @annotation.tailrec
    def findPivot(start: Int, end: Int): Int =
      val pivot = nums(end)
      var i     = start
      (start to end).foreach { j =>
        if nums(j) <= pivot then
          swap(i, j)
          i += 1
      }
      if i - 1 == nums.length - k then pivot
      else if i - 1 < nums.length - k then findPivot(i, end)
      else findPivot(start, i - 2)

    findPivot(0, nums.length - 1)
