package com.leetcode.cosminci._2600

import scala.collection.mutable
import scala.util.chaining.*

object _2597_NumBeautifulSubsets:

  def beautifulSubsets(nums: Array[Int], k: Int): Int =
    val counter = nums.groupMapReduce(identity)(_ => 1)(_ + _)

    val mem = mutable.Map.empty[Int, Int]
    def dfs(n: Int): Int = mem.getOrElseUpdate(n, {
      val cnt = math.pow(2, counter(n)).toInt - 1
      if !counter.contains(n - k) then cnt
      else cnt + dfs(n - k) + Option.when(counter.contains(n - 2 * k))(dfs(n - 2 * k) * cnt).getOrElse(0)
    })

    counter.keysIterator.filterNot(n => counter.contains(n + k)).map(dfs(_) + 1).product - 1
