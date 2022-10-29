package com.leetcode.cosminci._2400

import com.leetcode.cosminci._200._191_NumberOfOneBits.hammingWeight

object _2354_NumExcellentPairs:

  def countExcellentPairs(nums: Array[Int], k: Int): Long =
    val hwCounter = nums.distinct.groupMapReduce(hammingWeight)(_ => 1L)(_ + _)

    val counts = for
      (hw1, cnt1) <- hwCounter
      (hw2, cnt2) <- hwCounter
      if hw1 + hw2 >= k
    yield cnt1 * cnt2

    counts.sum
