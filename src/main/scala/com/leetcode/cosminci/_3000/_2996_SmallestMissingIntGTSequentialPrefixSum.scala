package com.leetcode.cosminci._3000

object _2996_SmallestMissingIntGTSequentialPrefixSum:

  def missingInteger(nums: Array[Int]): Int =
    val sum = nums.indices.tail.takeWhile(i => nums(i) == nums(i - 1) + 1).map(nums).sum + nums.head
    val set = nums.toSet
    Iterator.iterate(sum)(_ + 1).find(i => !set.contains(i)).getOrElse(-1)
