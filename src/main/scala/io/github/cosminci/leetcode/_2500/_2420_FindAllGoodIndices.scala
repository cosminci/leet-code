package io.github.cosminci.leetcode._2500

object _2420_FindAllGoodIndices:

  def goodIndices(nums: Array[Int], k: Int): List[Int] =
    val nonInc = nums.indices.tail.scanLeft(1) { (nonInc, i) =>
      if nums(i) <= nums(i - 1) then nonInc + 1 else 1
    }
    val nonDec = nums.indices.dropRight(1).scanRight(1) { (i, nonDec) =>
      if nums(i) <= nums(i + 1) then nonDec + 1 else 1
    }

    (k until nums.length - k)
      .filter(i => nonInc(i - 1) >= k && nonDec(i + 1) >= k)
      .toList
