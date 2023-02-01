package com.leetcode.cosminci._1100

import com.leetcode.cosminci.utils

object _1071_GcdOfStrings:

  def gcdOfStrings(s1: String, s2: String): String =
    if s1 ++ s2 != s2 ++ s1 then ""
    else s1.take(utils.gcd[Int](s1.length, s2.length))
