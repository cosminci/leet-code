package com.leetcode.cosminci._3100

object _3014_MinPushesToTypeWordsI:

  def minimumPushes(word: String): Int =
    word.distinct
      .grouped(8).zipWithIndex
      .map { case (group, i) => group.length * (i + 1) }.sum
