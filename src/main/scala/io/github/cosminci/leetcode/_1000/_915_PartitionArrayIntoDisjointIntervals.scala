package io.github.cosminci.leetcode._1000

object _915_PartitionArrayIntoDisjointIntervals:

  def main(args: Array[String]): Unit =
    println(partitionDisjoint(Array(5, 0, 3, 8, 6, 4, 10)))
    println(partitionDisjoint(Array(1, 1, 1, 0, 6, 12)))

  private def partitionDisjoint(nums: Array[Int]): Int =
    var length                = 1
    var (leftMax, overallMax) = (nums.head, nums.head)
    (1 to nums.length - 2).foreach { i =>
      if nums(i) < leftMax then
        length = i + 1
        leftMax = overallMax
      else overallMax = math.max(overallMax, nums(i))
    }
    length
