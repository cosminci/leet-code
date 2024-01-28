package com.leetcode.cosminci._3100

object _2030_FindMaxNumElemsInSubset:

  def maximumLength(nums: Array[Int]): Int =
    val freq = nums.sorted.foldRight(Map.empty[Int, Int]) { (n, freq) =>
      if freq.contains(n * n) && freq.contains(n) && n != 1 then freq.updated(n, freq(n * n) + 2)
      else freq.updated(n, 1)
    }
    val ones = nums.count(_ == 1)
    freq.values.max.max(ones - (if ones % 2 == 0 then 1 else 0))
