package com.leetcode.cosminci._2300

object _2278_PercentageOfLetterInString:

  def percentageLetter(s: String, letter: Char): Int =
    s.count(_ == letter) * 100 / s.length
