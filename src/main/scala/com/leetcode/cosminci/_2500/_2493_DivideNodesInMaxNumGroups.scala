package com.leetcode.cosminci._2500

import com.leetcode.cosminci.utils.UnionFind

import scala.collection.mutable

object _2493_DivideNodesInMaxNumGroups:

  def magnificentSets(n: Int, edges: Array[Array[Int]]): Int =
    val uf         = new UnionFind[Int]
    val emptyGraph = Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)
    val graph = edges.foldLeft(emptyGraph) { case (graph, Array(x, y)) =>
      uf.union(x, y)
      graph.updated(x, graph(x) :+ y).updated(y, graph(y) :+ x)
    }

    def bfs(node: Int): Option[Int] =
      val toVisit  = mutable.Queue((node, 1))
      val visited  = mutable.Map((node, 1))
      var maxLevel = 1
      while toVisit.nonEmpty do
        val (curr, level) = toVisit.dequeue()
        maxLevel = level
        graph(curr).foreach { next =>
          visited.get(next) match
            case Some(prevLevel) =>
              if prevLevel == level then return None
            case None =>
              visited.update(next, level + 1)
              toVisit.append((next, level + 1))
        }
      Some(maxLevel)

    (1 to n)
      .foldLeft(Map.empty[Int, Int].withDefaultValue(0)) { (maxGroupsPerRoot, node) =>
        bfs(node) match
          case None => return -1
          case Some(maxGroups) =>
            maxGroupsPerRoot.updated(uf.find(node), maxGroupsPerRoot(uf.find(node)).max(maxGroups))
      }.values.sum
