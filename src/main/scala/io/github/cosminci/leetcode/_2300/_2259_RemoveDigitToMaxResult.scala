package io.github.cosminci.leetcode._2300

object _2259_RemoveDigitToMaxResult:

  def removeDigit(n: String, digit: Char): String =
    val indices = n.indices.filter(i => n(i) == digit)

    val indexToDrop =
      if indices.length == 1 then indices.head
      else indices.filterNot(_ == n.length - 1).find(i => n(i) < n(i + 1)).getOrElse(indices.last)

    n.substring(0, indexToDrop) ++ n.substring(indexToDrop + 1)
