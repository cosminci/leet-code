package com.leetcode.cosminci._1500

import com.leetcode.cosminci.utils.UnionFind

import scala.util.chaining.*

object _1489_FindCriticalAndPseudoCriticalEdgesInMST:

  def findCriticalAndPseudoCriticalEdges(n: Int, edges: Array[Array[Int]]): List[List[Int]] =
    val edgeList = edges.zipWithIndex
      .map { case (Array(u, v, w), i) => (u, v, w, i) }
      .sortBy { case (_, _, w, _) => w }

    def kruskal(uf: UnionFind[Int], initialMst: Int, edge: Int) =
      val mst = edgeList.zipWithIndex.foldLeft(initialMst) { case (mst, ((u, v, w, _), i)) =>
        if i == edge then mst
        else if uf.find(u) == uf.find(v) then mst
        else uf.union(u, v).pipe(_ => mst + w)
      }
      if (0 until n).forall(uf.find(_) == uf.find(0)) then mst else Int.MaxValue

    def findMSTWithoutEdge(edge: Int) =
      kruskal(new UnionFind[Int], initialMst = 0, edge)

    def findMSTWithEdge(edge: Int) =
      val (u0, v0, w0, _) = edgeList(edge)
      kruskal(new UnionFind[Int].tap(_.union(u0, v0)), initialMst = w0, edge)

    def initialIndex(edge: Int) = edgeList(edge).pipe { case (_, _, _, i) => i }

    val base = findMSTWithoutEdge(edge = -1)
    val (critical, pseudoCritical) = edges.indices.foldLeft(Set.empty[Int], Set.empty[Int]) {
      case ((critical, pseudoCritical), edge) =>
        val (excl, incl) = (findMSTWithoutEdge(edge), findMSTWithEdge(edge))
        if excl > base then (critical + initialIndex(edge), pseudoCritical)
        else if incl == base then (critical, pseudoCritical + initialIndex(edge))
        else (critical, pseudoCritical)
    }

    List(critical.toList, pseudoCritical.toList)
