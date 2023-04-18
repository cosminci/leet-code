package com.leetcode.cosminci._1800

object _1768_MergeStringsAlternately:

  def mergeAlternately(word1: String, word2: String): String =
    word1
      .zipAll(word2, '_', '_')
      .flatMap { case (a, b) => s"$a$b".filterNot(_ == '_') }
      .mkString
