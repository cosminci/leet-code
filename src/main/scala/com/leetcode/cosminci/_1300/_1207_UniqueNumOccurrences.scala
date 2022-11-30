package com.leetcode.cosminci._1300

import scala.util.chaining.*

object _1207_UniqueNumOccurrences:

  def uniqueOccurrences(arr: Array[Int]): Boolean =
    arr
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .values
      .toSeq
      .pipe(counts => counts.size == counts.distinct.size)
