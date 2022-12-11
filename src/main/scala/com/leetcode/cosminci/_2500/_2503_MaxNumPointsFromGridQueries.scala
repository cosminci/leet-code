package com.leetcode.cosminci._2500

import com.leetcode.cosminci.utils.UnionFind

import scala.collection.mutable
import scala.util.chaining.*

object _2503_MaxNumPointsFromGridQueries:

  def maxPoints(grid: Array[Array[Int]], queries: Array[Int]): Array[Int] =
    val (m, n) = (grid.length, grid.head.length)
    val cells  = (0 until m).flatMap(x => (0 until n).map(y => (grid(x)(y), x, y))).sorted

    val uf = new UnionFind[Int]

    queries.zipWithIndex.sorted
      .foldLeft(Seq.fill(queries.length)(0), 0, Set.empty[(Int, Int)]) {
        case ((results, startCell, visited), (query, queryIdx)) =>
          val cellsToUse = cells.slice(startCell, cells.length).takeWhile { case (value, _, _) => value < query }
          cellsToUse
            .foldLeft(visited) { case (visited, (_, x, y)) =>
              Seq((0, 1), (0, -1), (1, 0), (-1, 0))
                .map { case (dx, dy) => (x + dx, y + dy) }
                .filter { case (nx, ny) => nx >= 0 && ny >= 0 && nx < m && ny < n && visited(nx -> ny) }
                .foreach { case (nx, ny) => uf.union(n * x + y, n * nx + ny) }
              visited + (x -> y)
            }
            .pipe { visited =>
              val result = Option.when(visited(0 -> 0))(uf.rank(uf.find(0))).getOrElse(0)
              (results.updated(queryIdx, result), startCell + cellsToUse.length, visited)
            }
      }.pipe { case (results, _, _) => results.toArray }
