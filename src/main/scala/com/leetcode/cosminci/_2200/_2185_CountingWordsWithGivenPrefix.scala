package com.leetcode.cosminci._2200

object _2185_CountingWordsWithGivenPrefix:

  def prefixCount(words: Array[String], pref: String): Int =
    words.count(_.startsWith(pref))
