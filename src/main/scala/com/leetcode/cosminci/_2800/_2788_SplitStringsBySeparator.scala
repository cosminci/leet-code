package com.leetcode.cosminci._2800

object _2788_SplitStringsBySeparator:

  def splitWordsBySeparator(words: List[String], separator: Char): List[String] =
    words.flatMap(_.split(separator)).filterNot(_.isEmpty)
