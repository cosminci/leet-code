package io.github.cosminci.leetcode._400

import scala.util.Random

object _398_RandomPickIndex:
  class Solution(nums: Array[Int]):
    private val numToIndices = nums.zipWithIndex.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    def pick(target: Int): Int =
      val candidates = numToIndices(target)
      candidates(Random.nextInt(candidates.length))
