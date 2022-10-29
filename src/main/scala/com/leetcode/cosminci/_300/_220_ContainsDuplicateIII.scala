package com.leetcode.cosminci._300

import scala.collection.mutable

object _220_ContainsDuplicateIII {
  def main(args: Array[String]): Unit = {
    println(containsNearbyAlmostDuplicate(Array(1, 2, 3, 1), 3, 0))
    println(containsNearbyAlmostDuplicate(Array(1, 0, 1, 1), 1, 2))
    println(containsNearbyAlmostDuplicate(Array(1, 5, 9, 1, 5, 9), 2, 3))
    println(containsNearbyAlmostDuplicate(Array(Int.MinValue, Int.MaxValue), 1, 1))
  }

  def containsNearbyAlmostDuplicate(nums: Array[Int], k: Int, t: Int): Boolean = {
    val slidingWindow = {
      given Ordering[Int] = (i, j) => nums(i).compare(nums(j))
      mutable.TreeSet.empty[Int]
    }

    nums.indices.exists { i =>
      if (slidingWindow.size > k) slidingWindow.remove(i - k - 1)
      val closest = Seq(slidingWindow.maxBefore(i), slidingWindow.minAfter(i)).flatten
      slidingWindow.add(i)
      closest.exists(j => math.abs(nums(i).toLong - nums(j)) <= t)
    }
  }
}
