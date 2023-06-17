package com.leetcode.cosminci._1200

import scala.collection.immutable.TreeSet
import scala.collection.mutable

object _1187_MakeArrayStrictlyIncreasing:

  def makeArrayIncreasing(arr1: Array[Int], arr2: Array[Int]): Int =
    val (min, max) = (Int.MinValue, Int.MaxValue.toLong)
    val candidates = TreeSet.from(arr2)

    val mem = mutable.Map.empty[(Int, Int), Long]
    def dfs(i: Int, needHigherThan: Int): Long = mem.getOrElseUpdate((i, needHigherThan),
      if i == arr1.length then 0L
      else
        val dontReplace = Option.when(arr1(i) > needHigherThan)(dfs(i + 1, arr1(i))).getOrElse(max)
        val doReplace   = candidates.minAfter(needHigherThan + 1).map(dfs(i + 1, _) + 1).getOrElse(max)
        dontReplace.min(doReplace)
    )

    val res = dfs(i = 0, needHigherThan = min)
    if res == max then -1 else res.toInt
