package com.leetcode.cosminci._2700

import scala.util.chaining.*

object _2609_FindLongestBalancedSubstringInBinaryString:

  def findTheLongestBalancedSubstring(s: String): Int =
    Iterator
      .iterate("01")(longest => s"0${longest}1")
      .dropWhile(s.contains).next()
      .pipe(_.length - 2)
