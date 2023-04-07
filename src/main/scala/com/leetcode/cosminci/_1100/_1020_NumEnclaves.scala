package com.leetcode.cosminci._1100

import scala.util.chaining.*

object _1020_NumEnclaves:

  def numEnclaves(grid: Array[Array[Int]]): Int =
    val (m, n) = (grid.length - 1, grid.head.length - 1)

    def explore(i: Int, j: Int, count: Int, visited: Set[(Int, Int)]): (Int, Set[(Int, Int)]) =
      if visited.contains(i -> j) then (count, visited)
      else Seq((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1))
        .filter { case (ni, nj) => grid(ni)(nj) == 1 }
        .filterNot { case (ni, nj) => ni < 0 || ni > m || nj < 0 || nj > n }
        .foldLeft((count - 1, visited + (i -> j))) { case ((c, v), (ni, nj)) => explore(ni, nj, c, v) }

    val (toVisit, count) = grid.indices.foldLeft(Seq.empty[(Int, Int)], 0) { case ((toVisit, count), i) =>
      grid(i).indices.foldLeft((toVisit, count)) { case ((toVisit, count), j) =>
        if grid(i)(j) == 0 then (toVisit, count)
        else (toVisit ++ Option.when(i == 0 || i == m || j == 0 || j == n)(i -> j), count + 1)
      }
    }

    Iterator
      .iterate((toVisit, count, Set.empty[(Int, Int)])) { case ((i -> j) +: toVisit, count, visited) =>
        explore(i, j, count, visited).pipe { case (count, visited) => (toVisit, count, visited) }
      }
      .dropWhile { case (toVisit, _, _) => toVisit.nonEmpty }.next()
      .pipe { case (_, count, _) => count }
