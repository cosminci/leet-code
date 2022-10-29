package com.leetcode.cosminci._100

object _28_ImplementStrStr:

  def strStr(haystack: String, needle: String): Int =
    Option
      .when(needle.length > haystack.length)(-1)
      .getOrElse(
        haystack
          .sliding(needle.length)
          .zipWithIndex
          .collectFirst { case (ss, i) if ss == needle => i }
          .getOrElse(-1)
      )
