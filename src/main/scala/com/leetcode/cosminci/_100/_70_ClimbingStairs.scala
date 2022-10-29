package com.leetcode.cosminci._100

object _70_ClimbingStairs:
  def main(args: Array[String]): Unit =
    println(climbStairs(3))

  def climbStairs(n: Int): Int =
    if (n == 1) 1
    else (3 to n).foldLeft(1, 2) { case ((prev2, prev1), _) =>
      (prev1, prev2 + prev1)
    }._2
