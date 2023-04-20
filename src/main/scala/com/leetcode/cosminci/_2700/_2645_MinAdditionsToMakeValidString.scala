package com.leetcode.cosminci._2700

import scala.util.chaining.*

object _2645_MinAdditionsToMakeValidString:

  def addMinimum(word: String): Int =
    word
      .foldLeft(0, 'z') { case ((k, prev), curr) => (k + (if curr <= prev then 1 else 0), curr) }
      .pipe { case (k, _) => k * 3 - word.length }
