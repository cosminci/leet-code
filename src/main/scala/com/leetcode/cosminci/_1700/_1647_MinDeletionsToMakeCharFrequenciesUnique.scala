package com.leetcode.cosminci._1700

import com.leetcode.cosminci.utils

import scala.collection.mutable

object _1647_MinDeletionsToMakeCharFrequenciesUnique:

  def minDeletions(s: String): Int =
    s.length - s.groupMapReduce(identity)(_ => 1)(_ + _)
      .values
      .toArray
      .sorted
      .foldRight(Int.MaxValue, 0) { case (freq, (prev, keep)) =>
        val unusedFreq = freq.min(prev - 1)
        if unusedFreq == 0 then (prev, keep)
        else (unusedFreq, keep + unusedFreq)
      }._2
