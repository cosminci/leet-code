package com.leetcode.cosminci._2600

object _2522_PartitionStrIntoSubstrWithValuesMaxK:

  def minimumPartition(s: String, k: Int): Int =
    s.map(_ - '0').foldLeft(1, 0L) { case ((cnt, curr), digit) =>
      if digit > k then return -1
      val next = curr * 10 + digit
      if next > k then (cnt + 1, digit) else (cnt, next)
    }._1
