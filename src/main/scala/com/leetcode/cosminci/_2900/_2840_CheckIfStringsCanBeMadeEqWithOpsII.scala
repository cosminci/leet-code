package com.leetcode.cosminci._2900

import scala.util.chaining.*

object _2840_CheckIfStringsCanBeMadeEqWithOpsII:

  def checkStrings(s1: String, s2: String): Boolean =
    def count(chars: Seq[Char]) = chars.groupMapReduce(identity)(_ => 1)(_ + _)
    def charCountsByIndexParity(s: String) =
      s.zipWithIndex
        .partitionMap { case (ch, i) => Either.cond(i % 2 == 0, ch, ch) }
        .pipe { case (even, odd) => (count(even), count(odd)) }

    val (even1, odd1) = charCountsByIndexParity(s1)
    val (even2, odd2) = charCountsByIndexParity(s2)

    even1 == even2 && odd1 == odd2
