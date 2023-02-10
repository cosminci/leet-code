package com.leetcode.cosminci._1200

import com.leetcode.cosminci.utils

import scala.util.chaining.*

object _1162_AsFarFromLandAsPossible:

  def maxDistance(grid: Array[Array[Int]]): Int =
    val toVisit = for
      x <- grid.indices
      y <- grid(x).indices
      if grid(x)(y) == 1
    yield (x, y)

    Iterator
      .iterate((toVisit, toVisit.toSet, -1)) { case (toVisit, visited, distance) =>
        toVisit.foldLeft(IndexedSeq.empty[(Int, Int)], visited) { case ((nextToVisit, visited), (x, y)) =>
          utils
            .neighbours(x, y, grid)
            .filterNot(visited.contains)
            .foldLeft(nextToVisit, visited) { case ((nextToVisit, visited), (nx, ny)) =>
              (nextToVisit :+ (nx, ny), visited + ((nx, ny)))
            }
          }.pipe { case (nextToVisit, visited) => (nextToVisit, visited, distance + 1) }
      }
      .dropWhile { case (toVisit, _, _) => toVisit.nonEmpty }.next()
      .pipe { case (_, _, distance) => if distance == 0 then -1 else distance }
