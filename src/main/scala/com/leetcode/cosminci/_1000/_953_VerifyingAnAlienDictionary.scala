package com.leetcode.cosminci._1000

object _953_VerifyingAnAlienDictionary:

  def isAlienSorted(words: Array[String], order: String): Boolean =
    words
      .sortBy(_.map(ch => ('a' + order.indexOf(ch)).toChar).mkString)
      .sameElements(words)
