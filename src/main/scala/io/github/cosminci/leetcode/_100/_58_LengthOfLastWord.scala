package io.github.cosminci.leetcode._100

object _58_LengthOfLastWord:
  def main(args: Array[String]): Unit =
    println(lengthOfLastWord("a"))

  def lengthOfLastWord(s: String): Int =
    var (idx, length) = (s.length - 1, 0)

    while s(idx) == ' ' do idx -= 1
    while idx >= 0 && s(idx) != ' ' do
      idx -= 1
      length += 1

    length
