package io.github.cosminci.leetcode._2300

object _2243_CalculateDigitSumOfString:

  def digitSum(s: String, k: Int): String =
    Iterator
      .iterate(s)(s => s.grouped(k).map(g => g.map(_ - '0').sum).mkString)
      .dropWhile(_.length > k)
      .next()
