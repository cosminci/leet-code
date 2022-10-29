package com.leetcode.cosminci._2000

object _1984_MinDifferenceBetweenHigherAndLowestOfKScores {
  def main(args: Array[String]): Unit = {
    println(minimumDifference(Array(90), 1))
    println(minimumDifference(Array(9, 4, 1, 7), 2))
  }

  def minimumDifference(nums: Array[Int], k: Int): Int =
    nums.sorted.sliding(k).map(window => window.last - window.head).min
}
