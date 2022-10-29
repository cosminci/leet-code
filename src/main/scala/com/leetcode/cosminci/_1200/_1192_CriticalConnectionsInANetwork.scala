package com.leetcode.cosminci._1200

import scala.collection.mutable

object _1192_CriticalConnectionsInANetwork:

  def criticalConnections(n: Int, connections: List[List[Int]]): List[List[Int]] =
    val graph = connections.foldLeft(Map.empty[Int, Array[Int]].withDefaultValue(Array.empty[Int])) {
      case (graph, conn) =>
        graph
          .updated(conn.head, graph(conn.head) :+ conn.last)
          .updated(conn.last, graph(conn.last) :+ conn.head)
    }
    val visitTs      = Array.ofDim[Int](n)
    val criticalConn = mutable.ListBuffer.empty[List[Int]]

    def dfs(node: Int, parent: Int, ts: Int): (Int, Int) =
      if visitTs(node) != 0 then return (visitTs(node), ts)
      visitTs(node) = ts
      val (minTs, newTs) = graph(node).foldLeft(Int.MaxValue, ts + 1) { case ((minTs, ts), nei) =>
        if nei == parent then (minTs, ts)
        else
          val (neiMinTs, neiTs) = dfs(nei, node, ts)
          (minTs.min(neiMinTs), neiTs)
      }
      if minTs >= visitTs(node) && parent >= 0 then criticalConn.append(List(parent, node))
      (minTs.min(visitTs(node)), newTs + 1)

    dfs(node = 0, parent = -1, ts = 1)
    criticalConn.toList
