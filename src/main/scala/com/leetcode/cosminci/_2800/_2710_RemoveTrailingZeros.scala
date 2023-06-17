package com.leetcode.cosminci._2800

object _2710_RemoveTrailingZeros {

  def removeTrailingZeros(num: String): String =
    num.reverse.dropWhile(_ == '0').reverse

}
