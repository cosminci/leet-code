package com.leetcode.cosminci._3100

object _3016_MinPushesToTypeWordsII:

  def minimumPushes(word: String): Int =
    word
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .values.toSeq.sortBy(-_)
      .grouped(8).zipWithIndex
      .map { case (group, i) => group.sum * (i + 1) }
      .sum
