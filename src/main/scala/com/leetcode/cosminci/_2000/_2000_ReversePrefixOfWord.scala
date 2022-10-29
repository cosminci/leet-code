package com.leetcode.cosminci._2000

object _2000_ReversePrefixOfWord:
  def main(args: Array[String]): Unit =
    println(reversePrefix("abcdefd", 'd'))

  def reversePrefix(word: String, ch: Char): String =
    word.indexOf(ch) match
      case -1 => word
      case i =>
        val (fh, sh) = word.splitAt(i + 1)
        s"${fh.reverse}$sh"
