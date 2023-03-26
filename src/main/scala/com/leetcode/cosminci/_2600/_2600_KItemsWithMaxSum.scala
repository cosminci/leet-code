package com.leetcode.cosminci._2600

object _2600_KItemsWithMaxSum:

  def kItemsWithMaximumSum(numOnes: Int, numZeros: Int, numNegOnes: Int, k: Int): Int =
    k.min(numOnes) - (k - numOnes - numZeros).max(0)
