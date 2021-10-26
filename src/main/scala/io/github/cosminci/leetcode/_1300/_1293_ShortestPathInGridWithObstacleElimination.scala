package io.github.cosminci.leetcode._1300

import io.github.cosminci.utils

import scala.collection.mutable

object _1293_ShortestPathInGridWithObstacleElimination {
  private def shortestPath(grid: Array[Array[Int]], k: Int): Int = {
    val toVisit = mutable.Queue((0, 0, 0, k))
    val visited = mutable.Map((0, 0) -> k)
    val target = (grid.length - 1, grid.head.length - 1)

    while (toVisit.nonEmpty) {
      val (x, y, steps, kLeft) = toVisit.dequeue()
      if ((x, y) == target) return steps

      utils.neighbours(x, y, grid).foreach {
        case (nx, ny) =>
          val newKLeft = if (grid(nx)(ny) == 1) kLeft - 1 else kLeft
          if (newKLeft >= 0 && visited.get((nx, ny)).forall(_ < newKLeft)) {
            toVisit.enqueue((nx, ny, steps + 1, newKLeft))
            visited.update((nx, ny), newKLeft)
          }
      }
    }
    -1
  }
}
