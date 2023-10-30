package com.leetcode.cosminci._1400

object _1356_SortIntsByNumOf1Bits:

  def sortByBits(arr: Array[Int]): Array[Int] =
    arr.sortBy(i => (Integer.bitCount(i), i))
