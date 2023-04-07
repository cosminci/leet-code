package com.leetcode.cosminci._2700

import scala.collection.immutable.TreeSet
import scala.util.chaining.*

object _2612_MinReverseOps:

  // TODO: optimizations to cover the edge cases that TLE
  def minReverseOperations(n: Int, p: Int, banned: Array[Int], k: Int): Array[Int] =
    val bannedSet = banned.toSet
    val available = (0 until n).foldLeft(Array(TreeSet.empty[Int], TreeSet.empty[Int])) { case (available, curr) =>
      if curr == p || bannedSet.contains(curr) then available
      else available.updated(curr & 1, available(curr & 1) + curr)
    }

    Iterator
      .iterate((Seq(p), Map(p -> 0), available)) { case (curr +: toVisit, dist, available) =>
        val low  = (curr - k + 1).max(0).pipe(l => 2 * l + k - 1 - curr)
        val high = ((curr + k - 1).min(n - 1) - (k - 1)).pipe(h => 2 * h + k - curr)
        LazyList.from(available(low % 2).range(low, high)).foldLeft((toVisit, dist, available)) {
          case ((toVisit, dist, av), nei) =>
            (toVisit :+ nei, dist.updated(nei, dist(curr) + 1), av.updated(low % 2, av(low % 2) - nei))
        }
      }
      .dropWhile { case (toVisit, _, _) => toVisit.nonEmpty }.next()
      .pipe { case (_, dist, _) => (0 until n).map(dist.getOrElse(_, -1)).toArray }
