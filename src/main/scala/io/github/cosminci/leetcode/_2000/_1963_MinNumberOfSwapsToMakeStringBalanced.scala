package io.github.cosminci.leetcode._2000

object _1963_MinNumberOfSwapsToMakeStringBalanced:
  def main(args: Array[String]): Unit =
    println(minSwaps("][]["))
    println(minSwaps("]]][[["))

  private def minSwaps(s: String): Int =
    (s.foldLeft((0, 0)) { case ((stack, mismatches), char) =>
      if char == '[' then (stack + 1, mismatches)
      else if stack > 0 then (stack - 1, mismatches)
      else (0, mismatches + 1)
    }._2 + 1) / 2
