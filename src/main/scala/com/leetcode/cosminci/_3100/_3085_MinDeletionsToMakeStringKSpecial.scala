package com.leetcode.cosminci._3100

object _3085_MinDeletionsToMakeStringKSpecial:

  def minimumDeletions(word: String, k: Int): Int =
    val counts = word.groupMapReduce(identity)(_ => 1)(_ + _).values
    counts.map { x =>
      counts.map { a =>
        if a < x then a
        else 0.max(a - (x + k))
      }.sum
    }.min
