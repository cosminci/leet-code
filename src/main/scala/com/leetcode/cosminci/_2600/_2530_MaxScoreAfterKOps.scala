package com.leetcode.cosminci._2600

import scala.collection.immutable.TreeSet
import scala.util.chaining.*

object _2530_MaxScoreAfterKOps:

  def maxKelements(nums: Array[Int], k: Int): Long =
    Iterator
      .iterate((TreeSet.from(nums.zipWithIndex), 0L)) { case (pool, score) =>
        val toRemove @ (max, i) = pool.last
        val toAddBack           = ((max + 2) / 3, i)
        (pool - toRemove + toAddBack, score + max)
      }
      .drop(k).next()
      .pipe { case (_, score) => score }
