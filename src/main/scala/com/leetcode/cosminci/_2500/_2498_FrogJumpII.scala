package com.leetcode.cosminci._2500

object _2498_FrogJumpII:

  def maxJump(stones: Array[Int]): Int =
    stones.drop(2).zip(stones)
      .map { case (j, i) => j - i }
      .maxOption
      .getOrElse(stones(1))
