package com.leetcode.cosminci._2700

import scala.util.chaining.*

object _2646_MinimizeTotalPriceOfTrips:

  def minimumTotalPrice(n: Int, edges: Array[Array[Int]], price: Array[Int], trips: Array[Array[Int]]): Int =
    val graph = edges.foldLeft(Map.empty[Int, Array[Int]].withDefaultValue(Array.empty[Int])) {
      case (graph, Array(a, b)) =>
        graph.updated(a, graph(a) :+ b).updated(b, graph(b) :+ a)
    }

    def dfs1(curr: Int, prev: Int, target: Int, count: Map[Int, Int]): (Map[Int, Int], Boolean) =
      if curr == target then (count.updated(curr, count(curr) + 1), true)
      else graph(curr)
        .filterNot(_ == prev)
        .foldLeft(count) { (count, child) =>
          dfs1(child, curr, target, count).pipe { case (count, found) =>
            if found then return (count.updated(curr, count(curr) + 1), true)
            else count
          }
        }
        .pipe(count => (count, false))

    val count = trips.foldLeft(Map.empty[Int, Int].withDefaultValue(0)) { case (count, Array(a, b)) =>
      dfs1(curr = a, prev = -1, target = b, count).pipe { case (count, _) => count }
    }

    val contribution = (0 until n).map(i => count(i) * price(i))

    def dfs2(curr: Int, prev: Int): (Int, Int) =
      graph(curr).filterNot(_ == prev).foldLeft((contribution(curr) / 2, contribution(curr))) {
        case ((half, full), child) =>
          val (childHalf, childFull) = dfs2(child, curr)
          (half + childFull, full + childHalf.min(childFull))
      }

    dfs2(curr = 0, prev = -1).pipe { case (half, full) => half.min(full) }
