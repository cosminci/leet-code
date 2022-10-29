package com.leetcode.cosminci._700

import com.leetcode.cosminci.utils.UnionFind

import scala.collection.mutable

object _684_RedundantConnection:

  def main(args: Array[String]): Unit =
    println(
      findRedundantConnection(
        Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(1, 4), Array(1, 5))
      ).toList
    )

  def findRedundantConnection(edges: Array[Array[Int]]): Array[Int] =
    val uf = new UnionFind[Int]
    edges.find { case edge @ Array(from, to) =>
      val (p1, p2) = (uf.find(from), uf.find(to))
      if (p1 == p2) true
      else {
        uf.union(p1, p2)
        false
      }
    }.get
