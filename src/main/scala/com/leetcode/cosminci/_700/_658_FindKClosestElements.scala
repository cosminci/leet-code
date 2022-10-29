package com.leetcode.cosminci._700

import scala.util.chaining.*

object _658_FindKClosestElements:

  def findClosestElements(arr: Array[Int], k: Int, x: Int): List[Int] =
    Iterator
      .iterate((0, arr.indices.last)) { case (l, r) =>
        if (x - arr(r)).abs >= (x - arr(l)).abs then (l, r - 1) else (l + 1, r)
      }
      .dropWhile { case (l, r) => r - l >= k }
      .next()
      .pipe { case (l, r) => arr.slice(l, r + 1).toList }
