package com.leetcode.cosminci._2700

import scala.util.chaining.*

object _2697_LexicoSmallestPalindrome:

  def makeSmallestPalindrome(s: String): String =
    s.take(s.length / 2)
      .zip(s.takeRight(s.length / 2).reverse)
      .map { case (a, b) => if a < b then a else b }
      .pipe { fh => fh ++ Option.when(s.length % 2 == 1)(s.charAt(s.length / 2)) ++ fh.reverse }
      .mkString
