package com.leetcode.cosminci._2700

import scala.util.chaining.*

object _2608_ShortestCycleInAGraph:

  def findShortestCycle(n: Int, edges: Array[Array[Int]]): Int =
    val graph = edges.foldLeft(Map.empty[Int, Array[Int]].withDefaultValue(Array.empty[Int])) {
      case (g, Array(a, b)) =>
        g.updated(a, g(a) :+ b).updated(b, g(b) :+ a)
    }

    def bfs(root: Int): Int =
      val distances = Map(root -> 0).withDefaultValue(-1)
      Iterator
        .iterate((Seq(root), distances, Int.MaxValue)) { case (curr +: toVisit, distances, minCycle) =>
          graph(curr).foldLeft((toVisit, distances, minCycle)) { case ((toVisit, distances, minCycle), next) =>
            if distances(next) == -1 then
              (toVisit :+ next, distances.updated(next, distances(curr) + 1), minCycle)
            else if distances(next) >= distances(curr) then
              (toVisit, distances, minCycle.min(distances(curr) + distances(next) + 1))
            else (toVisit, distances, minCycle)
          }
        }
        .dropWhile { case (toVisit, _, _) => toVisit.nonEmpty }.next()
        .pipe { case (_, _, minCycle) => minCycle }

    (0 until n).map(bfs).min.pipe(res => if res < Int.MaxValue then res else -1)
