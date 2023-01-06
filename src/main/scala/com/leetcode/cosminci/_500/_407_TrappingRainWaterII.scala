package com.leetcode.cosminci._500

import scala.collection.immutable.TreeSet
import scala.util.chaining.*

object _407_TrappingRainWaterII:

  def trapRainWater(heightMap: Array[Array[Int]]): Int =
    val (m, n) = (heightMap.length, heightMap.head.length)
    val border = for
      r <- 0 until m
      c <- 0 until n
      if r == 0 || c == 0 || r == m - 1 || c == n - 1
    yield (r, c)

    val toVisit = TreeSet.from(border.map { case (r, c) => (heightMap(r)(c), r, c) })

    Iterator
      .iterate((toVisit, border.toSet, 0, 0)) { case (toVisit, visited, prevLevel, result) =>
        val (height, r, c) = toVisit.head
        val level          = prevLevel.max(height)

        Seq((r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c))
          .filter { case nei @ (nr, nc) => nr >= 0 && nr < m && nc >= 0 && nc < n && !visited.contains(nei) }
          .foldLeft(toVisit.tail, visited, result) { case ((toVisit, visited, result), nei @ (nr, nc)) =>
            val waterTrapped = (level - heightMap(nr)(nc)).max(0)
            val neiToVisit   = (heightMap(nr)(nc), nr, nc)
            (toVisit + neiToVisit, visited + nei, result + waterTrapped)
          }
          .pipe { case (toVisit, visited, result) => (toVisit, visited, level, result) }
      }
      .dropWhile { case (toVisit, _, _, _) => toVisit.nonEmpty }.next()
      .pipe { case (_, _, _, result) => result }
