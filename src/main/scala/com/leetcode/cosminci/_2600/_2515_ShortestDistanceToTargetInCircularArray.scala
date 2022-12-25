package com.leetcode.cosminci._2600

object _2515_ShortestDistanceToTargetInCircularArray:

  def closetTarget(words: Array[String], target: String, startIndex: Int): Int =
    words.indices
      .filter(i => words(i) == target)
      .map(i => (i - startIndex).abs min words.length - (i - startIndex).abs)
      .minOption
      .getOrElse(-1)
