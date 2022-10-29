package com.leetcode.cosminci._400

object _344_ReverseString:

  def reverseString(s: Array[Char]): Unit =
    (0 until s.length / 2).foreach { i =>
      val tmp = s(i)
      s(i) = s(s.length - i - 1)
      s(s.length - i - 1) = tmp
    }
