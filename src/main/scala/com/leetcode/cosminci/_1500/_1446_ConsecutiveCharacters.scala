package com.leetcode.cosminci._1500

object _1446_ConsecutiveCharacters {
  def maxPower(s: String): Int =
    s.indices.tail.foldLeft(1, 1) {
      case ((max, curr), i) =>
        Option.when(s(i) == s(i - 1))(((curr + 1).max(max), curr + 1)).getOrElse((max, 1))
    }._1
}
