package com.leetcode.cosminci._1900

object _1832_CheckIfSentenceIsPangram:

  def checkIfPangram(sentence: String): Boolean =
    sentence.distinct.length == 26
