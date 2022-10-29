package com.leetcode.cosminci._700

object _659_SplitArrayIntoConsecutiveSubseqs:
  def main(args: Array[String]): Unit =
    println(isPossible(Array(1, 2, 3, 3, 4, 5)))
    println(isPossible(Array(1, 2, 3, 3, 4, 4, 5, 5)))
    println(isPossible(Array(1, 2, 3, 4, 4, 5)))

  def isPossible(nums: Array[Int]): Boolean =
    nums.foldLeft(Map.empty[Int, Int], nums.groupBy(identity).view.mapValues(_.length).toMap) {
      case ((subseqEndCounts, freqCounts), n) =>
        val newFreqCounts = freqCounts.updated(n, freqCounts(n) - 1)

        if freqCounts(n) == 0 then (subseqEndCounts, freqCounts)
        else if subseqEndCounts.getOrElse(n, 0) > 0 then
          (
            subseqEndCounts
              .updated(n, subseqEndCounts(n) - 1)
              .updated(n + 1, subseqEndCounts.getOrElse(n + 1, 0) + 1),
            newFreqCounts
          )
        else if freqCounts.getOrElse(n + 1, 0) > 0 && freqCounts.getOrElse(n + 2, 0) > 0 then
          (
            subseqEndCounts.updated(n + 3, subseqEndCounts.getOrElse(n + 3, 0) + 1),
            newFreqCounts
              .updated(n + 1, freqCounts(n + 1) - 1)
              .updated(n + 2, freqCounts(n + 2) - 1)
          )
        else return false
    }
    true
