package com.leetcode.cosminci._2700

import scala.util.chaining.*

object _2603_CollectCoinsInTree:

  def collectTheCoins(coins: Array[Int], edges: Array[Array[Int]]): Int =
    val graph = edges.foldLeft(Map.empty[Int, Array[Int]].withDefaultValue(Array.empty[Int])) {
      case (graph, Array(a, b)) =>
        graph.updated(a, graph(a) :+ b).updated(b, graph(b) :+ a)
    }

    val (toVisit, edgeCount) = coins.indices.foldLeft(Array.empty[Int], Map.empty[Int, Int]) {
      case ((toVisit, edgeCount), i) =>
        (toVisit ++ Option.when(graph(i).length == 1)(i), edgeCount.updated(i, graph(i).length))
    }

    val steps = Map.empty[Int, Int].withDefaultValue(Int.MaxValue)
    Iterator
      .iterate((toVisit, edgeCount, steps, coins.length)) { case (toVisit0, edgeCount, steps, nodeCount) =>
        val (curr, toVisit) = (toVisit0.head, toVisit0.tail)
        steps.updated(curr, steps(curr) - 1).pipe { steps =>
          if steps(curr) == 0 then (toVisit, edgeCount, steps, nodeCount)
          else graph(curr)
            .foldLeft(toVisit, edgeCount, steps) { case ((toVisit, edgeCount, steps), nei) =>
              val minSteps     = if coins(curr) > 0 then 2 else Int.MaxValue
              val newSteps     = steps.updated(nei, steps(nei).min(steps(curr)).min(minSteps))
              val newEdgeCount = edgeCount.updated(nei, edgeCount(nei) - 1)
              val newToVisit   = if newEdgeCount(nei) == 1 then toVisit :+ nei else toVisit
              (newToVisit, newEdgeCount, newSteps)
            }
            .pipe { case (toVisit, edgeCount, steps) => (toVisit, edgeCount, steps, nodeCount - 1) }
        }
      }
      .dropWhile { case (toVisit, _, _, _) => toVisit.nonEmpty }.next()
      .pipe { case (_, _, _, nodeCount) => 2 * (nodeCount - 1).max(0) }
