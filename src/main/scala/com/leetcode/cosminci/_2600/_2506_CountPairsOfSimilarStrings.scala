package com.leetcode.cosminci._2600

object _2506_CountPairsOfSimilarStrings:

  def similarPairs(words: Array[String]): Int =
    words
      .map(_.toSet)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .values
      .map(n => n * (n - 1) / 2)
      .sum
