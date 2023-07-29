package com.leetcode.cosminci._2800

import scala.collection.mutable
import scala.util.chaining.*

object _2791_CountPathsThatFormAPalindromeInTree:

  def countPalindromePaths(parent: List[Int], s: String): Long =
    val parents = parent.toArray

    val mem = mutable.Map.empty[Int, Int]
    def mask(i: Int): Int =
      mem.getOrElseUpdate(i, if i == 0 then 0 else mask(parents(i)) ^ (1 << s(i) - 'a'))

    parents.indices
      .foldLeft(0L, Map.empty[Int, Int].withDefaultValue(0)) { case ((res, cnt), i) =>
        val newRes = (0 until 26).foldLeft(res)((res, j) => res + cnt(mask(i) ^ (1 << j)))
        (newRes + cnt(mask(i)), cnt.updated(mask(i), cnt(mask(i)) + 1))
      }
      .pipe { case (res, _) => res }
