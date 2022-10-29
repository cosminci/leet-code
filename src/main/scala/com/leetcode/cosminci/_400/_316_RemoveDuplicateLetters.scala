package com.leetcode.cosminci._400

import com.leetcode.cosminci.utils

object _316_RemoveDuplicateLetters:
  def main(args: Array[String]): Unit =
    println(removeDuplicateLetters("cbacdcbc"))
    println(removeDuplicateLetters("bcabc"))

  def removeDuplicateLetters(s: String): String =
    s.foldLeft(s.groupMapReduce(identity)(_ => 1)(_ + _), "", Set.empty[Char]) {
      case ((counts, result, set), char) if set.contains(char) =>
        (counts.updated(char, counts(char) - 1), result, set)
      case ((counts, result, set), char) =>
        val (drop, keep) = result.span(c => c > char && counts(c) > 0)
        (counts.updated(char, counts(char) - 1), char +: keep, set.removedAll(drop) + char)
    }._2.reverse
