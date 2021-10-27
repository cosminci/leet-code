package io.github.cosminci.leetcode._700

import io.github.cosminci.utils.DisjointSetUnion.DSU

import scala.collection.mutable

object _684_RedundantConnection:

  def main(args: Array[String]): Unit =
    println(
      findRedundantConnection(
        Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(1, 4), Array(1, 5))
      ).toList
    )

  private def findRedundantConnection(edges: Array[Array[Int]]): Array[Int] =
    val dsu = new DSU
    edges.find { case edge @ Array(from, to) =>
      val (p1, p2) = (dsu.find(from), dsu.find(to))
      if (p1 == p2) true
      else {
        dsu.union(p1, p2)
        false
      }
    }.get