package com.leetcode.cosminci._1600

import com.leetcode.cosminci.utils.UnionFind

import scala.util.chaining.*

object _1579_RemoveMaxEdgesToKeepGraphTraversable:

  def maxNumEdgesToRemove(n: Int, edges: Array[Array[Int]]): Int =
    edges.sortInPlaceBy { case Array(kind, _, _) => -kind }

    val ufAlice = new UnionFind[Int]
    val ufBob   = new UnionFind[Int]

    edges
      .foldLeft(0, 0, 0) { case ((removedEdges, aliceEdges, bobEdges), Array(kind, u, v)) =>
        kind match
          case 3 =>
            if ufAlice.find(u) == ufAlice.find(v) then (removedEdges + 1, aliceEdges, bobEdges)
            else
              ufAlice.union(u, v)
              ufBob.union(u, v)
              (removedEdges, aliceEdges + 1, bobEdges + 1)
          case 2 =>
            if ufBob.find(u) == ufBob.find(v) then (removedEdges + 1, aliceEdges, bobEdges)
            else
              ufBob.union(u, v)
              (removedEdges, aliceEdges, bobEdges + 1)
          case 1 =>
            if ufAlice.find(u) == ufAlice.find(v) then (removedEdges + 1, aliceEdges, bobEdges)
            else
              ufAlice.union(u, v)
              (removedEdges, aliceEdges + 1, bobEdges)
      }
      .pipe { case (removedEdges, aliceEdges, bobEdges) =>
        if bobEdges == n - 1 && aliceEdges == n - 1 then removedEdges else -1
      }
