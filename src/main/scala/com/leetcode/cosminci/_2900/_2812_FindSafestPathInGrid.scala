package com.leetcode.cosminci._2900

import scala.collection.immutable.TreeSet
import scala.util.chaining.*

object _2812_FindSafestPathInGrid:

  def maximumSafenessFactor(grid: List[List[Int]]): Int =
    val n = grid.length
    val thieves = for
      x <- grid.indices
      y <- grid.indices
      if grid(x)(y) == 1
    yield (x, y)

    val (_, _, safeness) = Iterator
      .iterate((thieves.toSet, 0, Map.from(thieves.map(_ -> 0)))) { case (toVisit, dist, safeness) =>
        toVisit.flatMap { case (x, y) =>
          Seq((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y))
            .filter { case (nx, ny) => nx.min(ny) >= 0 && nx.max(ny) < n }
            .filter { case (nx, ny) => grid(nx)(ny) == 0 && !safeness.contains((nx, ny)) }
        }.pipe(newToVisit => (newToVisit, dist + 1, safeness ++ newToVisit.map(_ -> (dist + 1))))
      }.dropWhile { case (toVisit, _, _) => toVisit.nonEmpty }.next()

    val toVisit = TreeSet((0, 0, safeness((0, 0))))(Ordering.by { case (x, y, dist) => (-dist, -x, -y) })
    Iterator
      .iterate((toVisit, Map((0, 0) -> safeness((0, 0))))) { case (toVisit, visited) =>
        val (x, y, dist) = toVisit.head
        val newToVisit = Seq((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y))
          .filter { case (nx, ny) => nx.min(ny) >= 0 && nx.max(ny) < n && !visited.contains((nx, ny)) }
          .map { case (nx, ny) => (nx, ny, dist.min(safeness((nx, ny)))) }
          .filter { case (nx, ny, ndist) => visited.get((nx, ny)).forall(_ < ndist) }
        (toVisit.tail ++ newToVisit, visited + ((x, y) -> dist))
      }
      .dropWhile { case (toVisit, _) => toVisit.head.pipe { case (x, y, _) => x < n - 1 || y < n - 1 } }.next()
      .pipe { case (toVisit, _) => toVisit.head.pipe { case (_, _, dist) => dist } }
