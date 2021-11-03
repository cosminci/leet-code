package io.github.cosminci.leetcode._2100

object _2016_MaxDiffBetweenIncreasingElements:
  def main(args: Array[String]): Unit =
    println(maximumDifference(Array(7, 1, 5, 4)))
    println(maximumDifference(Array(9, 4, 3, 2)))
    println(maximumDifference(Array(1, 5, 2, 10)))

  def maximumDifference(nums: Array[Int]): Int =
    nums
      .foldLeft(Int.MaxValue, -1) { case ((prevMin, prevMaxDiff), n) =>
        if n <= prevMin then (n, prevMaxDiff)
        else (prevMin, math.max(prevMaxDiff, n - prevMin))
      }
      ._2
