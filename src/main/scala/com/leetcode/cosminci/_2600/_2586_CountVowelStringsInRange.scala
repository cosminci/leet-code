package com.leetcode.cosminci._2600

object _2586_CountVowelStringsInRange:

  def vowelStrings(words: Array[String], left: Int, right: Int): Int =
    (left to right).count { i =>
      Seq(words(i).head, words(i).last).forall("aeiou".contains(_))
    }
