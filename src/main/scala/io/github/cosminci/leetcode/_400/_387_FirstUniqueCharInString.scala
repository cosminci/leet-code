package io.github.cosminci.leetcode._400

object _387_FirstUniqueCharInString:

  def firstUniqChar(s: String): Int =
    s.groupMapReduce(identity)(_ => 1)(_ + _)
      .collect { case (char, count) if count == 1 => char }
      .map(char => s.indexOf(char))
      .minOption
      .getOrElse(-1)
