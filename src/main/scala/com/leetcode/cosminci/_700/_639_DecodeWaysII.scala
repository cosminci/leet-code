package com.leetcode.cosminci._700

object _639_DecodeWaysII:
  def numDecodings(s: String): Int =
    val mod = 1_000_000_007

    var (first, second) = (1L, if s.head == '*' then 9L else if s.head == '0' then 0L else 1L)

    (1 until s.length).foreach { i =>
      val tmp                  = second
      val (currChar, prevChar) = (s.charAt(i), s.charAt(i - 1))

      if currChar == '*' then
        second = (9 * second) % mod
        if prevChar == '1' then second = (second + 9 * first) % mod
        else if prevChar == '2' then second = (second + 6 * first) % mod
        else if prevChar == '*' then second = (second + 15 * first) % mod
      else
        second = if currChar == '0' then 0 else second
        if prevChar == '1' then second = (second + first) % mod
        else if prevChar == '2' && currChar <= '6' then second = (second + first) % mod
        else if prevChar == '*' then second = (second + (if currChar <= '6' then 2 else 1) * first) % mod

      first = tmp
    }
    second.toInt
