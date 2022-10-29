package com.leetcode.cosminci._2300

object _2255_CountPrefixesOfAGivenString:

  def countPrefixes(words: Array[String], s: String): Int = words.count(s.startsWith)
