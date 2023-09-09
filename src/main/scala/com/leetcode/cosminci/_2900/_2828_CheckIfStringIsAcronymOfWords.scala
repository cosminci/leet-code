package com.leetcode.cosminci._2900

object _2828_CheckIfStringIsAcronymOfWords:

  def isAcronym(words: List[String], s: String): Boolean =
    words.map(_.head).mkString == s
