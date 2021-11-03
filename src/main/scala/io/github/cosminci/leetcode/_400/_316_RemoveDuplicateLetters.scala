package io.github.cosminci.leetcode._400

import io.github.cosminci.utils

object _316_RemoveDuplicateLetters:
  def main(args: Array[String]): Unit =
    println(removeDuplicateLetters("cbacdcbc"))
    println(removeDuplicateLetters("bcabc"))

  def removeDuplicateLetters(s: String): String =
    s.distinct.sorted.foreach { char =>
      val suffix = s.substring(s.indexOf(char))
      if suffix.toSet == s.toSet then return char +: removeDuplicateLetters(suffix.filter(_ != char))
    }
    ""
