package io.github.cosminci.leetcode._500

import io.github.cosminci.utils

import scala.collection.mutable

object _417_PacificAtlanticWaterFlow:

  def pacificAtlantic(heights: Array[Array[Int]]): List[List[Int]] =
    val (m, n) = (heights.indices.last, heights.head.indices.last)

    @annotation.tailrec
    def reachable(toVisit: Set[(Int, Int)], visited: Set[(Int, Int)]): Set[(Int, Int)] =
      toVisit.headOption match
        case None => visited
        case Some(coord @ (x, y)) =>
          val next = utils
            .neighbours(x, y, heights)
            .filter { case ncoord @ (nx, ny) => !visited.contains(ncoord) && heights(x)(y) <= heights(nx)(ny) }
          reachable(toVisit - coord ++ next, visited ++ next)

    val pacificSeed  = ((0 to m).map(x => (x, 0)) ++ (0 to n).map(y => (0, y))).toSet
    val atlanticSeed = ((0 to m).map(x => (x, n)) ++ (0 to n).map(y => (m, y))).toSet

    val pacificReachable  = reachable(toVisit = pacificSeed, visited = pacificSeed)
    val atlanticReachable = reachable(toVisit = atlanticSeed, visited = atlanticSeed)

    pacificReachable.intersect(atlanticReachable).map { case (x, y) => List(x, y) }.toList
