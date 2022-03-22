package io.github.cosminci.leetcode._1700

object _1663_SmallestStringWithAGivenNumericValue:
  def getSmallestString(n: Int, k: Int): String =
    val sb = new StringBuilder
    (0 until n).foldLeft(k) { (k, i) =>
      val char = (k - 26 * (n - i - 1)).max(1)
      sb.append((char + 'a' - 1).toChar)
      k - char
    }
    sb.mkString
