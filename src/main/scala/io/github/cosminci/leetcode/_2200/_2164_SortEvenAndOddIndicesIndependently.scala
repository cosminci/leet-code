package io.github.cosminci.leetcode._2200

object _2164_SortEvenAndOddIndicesIndependently:

  def sortEvenOdd(nums: Array[Int]): Array[Int] =
    val (odd, even) = nums.indices.partition(_ % 2 == 1)
    val sortedOdd   = odd.map(nums).sortBy(x => -x)
    val sortedEven  = even.map(nums).sortBy(x => x)
    Array.tabulate(nums.length) { idx =>
      if idx % 2 == 0 then sortedEven(idx / 2) else sortedOdd(idx / 2)
    }
