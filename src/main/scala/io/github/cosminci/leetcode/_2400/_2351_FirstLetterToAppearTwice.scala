package io.github.cosminci.leetcode._2400

object _2351_FirstLetterToAppearTwice:

  def repeatedCharacter(s: String): Char =
    s.foldLeft(0) { (seen, c) =>
      if (seen & 1 << (c - 'a')) > 0 then return c
      else seen | (1 << (c - 'a'))
    }.toChar
