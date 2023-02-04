package com.leetcode.cosminci._2600

object _2554_MaxNumIntsToChooseFromRangeI:

  def maxCount(banned: Array[Int], n: Int, maxSum: Int): Int =
    val banSet    = banned.toSet
    val prefixSum = (1 to n).filterNot(banSet.contains).scanLeft(0)(_ + _).tail
    prefixSum.takeWhile(_ <= maxSum).length
