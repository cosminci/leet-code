package com.leetcode.cosminci._2000

import scala.collection.mutable
import scala.util.chaining.*

object _1970_LastDayWhereYouCanStillCross:

  def latestDayToCross(m: Int, n: Int, cells: Array[Array[Int]]): Int =
    val flooded = cells.scanLeft(Set.empty[(Int, Int)]) { case (flooded, Array(r, c)) => flooded + ((r - 1, c - 1)) }

    def canReach(day: Int): Boolean =
      val startCells = (0 until n).map(c => (0, c)).filterNot(flooded(day).contains)
      Iterator
        .iterate((startCells, startCells.toSet)) { case ((r, c) +: toVisit, visited) =>
          val newToVisit = Seq((r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c))
            .filter { case (nr, nc) => nr >= 0 && nr < m && nc >= 0 && nc < n }
            .filterNot(visited.contains)
            .filterNot(flooded(day).contains)
          (toVisit ++ newToVisit, visited ++ newToVisit)
        }
        .dropWhile { case (toVisit, _) => toVisit.nonEmpty && toVisit.headOption.forall { case (r, _) => r != m - 1 } }
        .next().pipe { case (toVisit, _) => toVisit.headOption.exists { case (r, _) => r == m - 1 } }

    Iterator
      .iterate((1, cells.length)) { case (l, r) => ((l + r) / 2).pipe(m => if (canReach(m)) (m + 1, r) else (l, m)) }
      .dropWhile { case (l, r) => l < r }
      .next().pipe { case (l, _) => l - 1 }
