package com.leetcode.cosminci._3100

import scala.util.chaining.*

object _3067_CountPairsOfConnectableServersInWeightedTreeNetwork:

  def countPairsOfConnectableServers(edges: Array[Array[Int]], signalSpeed: Int): Array[Int] =
    val graph = edges.foldLeft(Map.empty[Int, Vector[(Int, Int)]]) { case (graph, Array(u, v, w)) =>
      graph
        .updated(u, graph.getOrElse(u, Vector.empty) :+ (v, w))
        .updated(v, graph.getOrElse(v, Vector.empty) :+ (u, w))
    }
    def dfs(node: Int, parent: Int, distance: Int): Int =
      graph(node).foldLeft(if distance % signalSpeed == 0 then 1 else 0) { case (count, (child, weight)) =>
        if child == parent then count
        else count + dfs(child, node, distance + weight)
      }

    Array.tabulate(graph.size) { bridge =>
      graph(bridge)
        .foldLeft((0, 0)) { case ((res, validNodes), (child, weight)) =>
          val newValidNodes = dfs(child, bridge, weight)
          (res + validNodes * newValidNodes, validNodes + newValidNodes)
        }.pipe { case (res, _) => res }
    }
