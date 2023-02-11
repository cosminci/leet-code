package com.leetcode.cosminci._1200

import scala.util.chaining.*

object _1129_ShortestPathWithAlternatingColors:

  def shortestAlternatingPaths(n: Int, redEdges: Array[Array[Int]], blueEdges: Array[Array[Int]]): Array[Int] =
    val graph0 = redEdges.foldLeft(Map.empty[Int, Seq[(Int, Boolean)]]) { case (graph, Array(a, b)) =>
      graph.updated(a, graph.getOrElse(a, Seq.empty) :+ (b, false))
    }
    val graph = blueEdges.foldLeft(graph0.withDefaultValue(Seq.empty)) { case (graph, Array(a, b)) =>
      graph.updated(a, graph.getOrElse(a, Seq.empty) :+ (b, true))
    }
    val toVisit = Seq((0, 0, false), (0, 0, true))
    val visited = Set((0, false), (0, true))

    Iterator
      .iterate((toVisit, visited, Map.empty[Int, Int].withDefaultValue(Int.MaxValue))) {
        case (toVisit, visited, result) =>
          val (node, steps, color) +: remaining = toVisit
          graph(node)
            .filterNot(visited.contains)
            .filter { case (_, neiColor) => neiColor ^ color }
            .foldLeft(remaining, visited, result) { case ((toVisit, visited, result), nei @ (node, color)) =>
              (toVisit :+ (node, steps + 1, color), visited + nei, result.updated(node, result(node).min(steps + 1)))
            }
      }
      .dropWhile { case (toVisit, _, _) => toVisit.nonEmpty }.next()
      .pipe { case (_, _, result) =>
        (0 until n).map { i =>
          result(i).pipe(v => if i == 0 then 0 else if v == Int.MaxValue then -1 else v)
        }
      }.toArray
