package io.github.cosminci.leetcode._1700

object _1662_CheckIfTwoStringArraysAreEq:

  def arrayStringsAreEqual(word1: Array[String], word2: Array[String]): Boolean =
    LazyList.from(word1).flatten == LazyList.from(word2).flatten
