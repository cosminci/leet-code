package io.github.cosminci.leetcode._100

object _91_DecodeWays:
  def main(args: Array[String]): Unit =
    println(numDecodings("123123"))
    println(numDecodings("10"))
    println(numDecodings("100"))
    println(numDecodings("01"))

  def numDecodings(s: String): Int =
    val dp = Array.fill[Int](s.length + 1)(1)

    (s.length - 1 to 0 by -1).foreach { idx =>
      dp(idx) = if s(idx) == '0' then 0 else dp(idx + 1)

      if idx + 1 < s.length && s(idx) != '0' && s"${s(idx)}${s(idx + 1)}".toInt <= 26 then dp(idx) += dp(idx + 2)
    }

    dp.head
