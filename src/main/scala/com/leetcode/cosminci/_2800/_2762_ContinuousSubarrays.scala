package com.leetcode.cosminci._2800

import scala.collection.immutable.TreeMap
import scala.util.chaining.*

object _2762_ContinuousSubarrays:

  def continuousSubarrays(nums: Array[Int]): Long =
    Iterator.iterate((0L, 0, 0, TreeMap.empty[Int, Int])) { case (res, i, j, cnt) =>
      Iterator.iterate((cnt.updated(nums(j), cnt.getOrElse(nums(j), 0) + 1), i)) { case (cnt, i) =>
        (cnt.updatedWith(nums(i)) {
          case Some(c) if c > 1 => Some(c - 1)
          case _ => None
        }, i + 1)
      }
      .dropWhile { case (cnt, _) => cnt.size > 1 && cnt.lastKey - cnt.firstKey > 2 }.next()
      .pipe { case (cnt, i) => (res + j - i + 1, i, j + 1, cnt) }
    }
    .dropWhile { case (_, _, j, _) => j < nums.length }.next()
    .pipe { case (res, _, _, _) => res }
