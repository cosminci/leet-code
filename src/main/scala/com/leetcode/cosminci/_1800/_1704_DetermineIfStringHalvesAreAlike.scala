package com.leetcode.cosminci._1800

import scala.util.chaining.*

object _1704_DetermineIfStringHalvesAreAlike:

  private val vowels = Set('a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U')

  def halvesAreAlike(s: String): Boolean =
    s.splitAt(s.length / 2)
      .pipe { case (fh, sh) => fh.count(vowels.contains) == sh.count(vowels.contains) }
