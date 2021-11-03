package io.github.cosminci.leetcode._1900

import scala.annotation.tailrec

object _1849_SplittingStringIntoDescendingConsecutiveValues:
  def main(args: Array[String]): Unit =
    println(splitString("1000000000999999999"))
    println(splitString("000937149937148"))
    println(splitString("10"))
    println(splitString("2109"))
    println(splitString("6442450944"))
    println(splitString("10009998"))
    println(splitString("001"))

  def splitString(s: String): Boolean =
    @tailrec
    def dfs(s: String, prev: BigInt): Boolean =
      if s.isEmpty then return true
      if prev == 1 then return BigInt(s) == 0

      var acc = BigInt(0)
      var i   = 0
      while acc < prev - 1 && i < s.length do
        acc = acc * 10 + (s(i) - '0')
        i += 1

      acc == prev - 1 && dfs(s.substring(i), acc)

    s.indices.exists { i =>
      val (prev, rem) = s.splitAt(i + 1)
      if rem.isEmpty then return false
      dfs(rem, BigInt(prev))
    }
