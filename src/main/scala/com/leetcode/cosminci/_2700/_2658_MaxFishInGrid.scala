package com.leetcode.cosminci._2700

import scala.util.chaining.*

object _2658_MaxFishInGrid:

  def findMaxFish(grid: Array[Array[Int]]): Int =
    def canVisit(visited: Set[(Int, Int)], r: Int, c: Int) =
      r >= 0 && r < grid.length && c >= 0 && c < grid(r).length && !visited.contains((r, c)) && grid(r)(c) > 0

    def dfs(visited: Set[(Int, Int)], r: Int, c: Int): (Int, Set[(Int, Int)]) =
      if !canVisit(visited, r, c) then (0, visited)
      else Seq((r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1))
        .foldLeft(grid(r)(c), visited + ((r, c))) { case ((res, visited), (nr, nc)) =>
          dfs(visited, nr, nc).pipe { case (nRes, visited) => (res + nRes, visited) }
        }

    val coords = for
      r <- grid.indices
      c <- grid(r).indices
    yield (r, c)

    coords.foldLeft(0, Set.empty[(Int, Int)]) { case ((res, visited), (r, c)) =>
      dfs(visited, r, c).pipe { case (localRes, visited) => (res.max(localRes), visited) }
    }.pipe { case (res, _) => res }
