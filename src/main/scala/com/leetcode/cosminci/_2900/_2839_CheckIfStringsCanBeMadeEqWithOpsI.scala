package com.leetcode.cosminci._2900

object _2839_CheckIfStringsCanBeMadeEqWithOpsI:

  def canBeEqual(s1: String, s2: String): Boolean =
    def partition(s: String) =
      s.zipWithIndex.partitionMap { case (ch, i) => Either.cond(i % 2 == 0, ch, ch) }

    val (even1, odd1) = partition(s1)
    val (even2, odd2) = partition(s2)

    even1.toSet == even2.toSet && odd1.toSet == odd2.toSet
