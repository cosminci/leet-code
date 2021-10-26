package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _684_RedundantConnection:

  def main(args: Array[String]): Unit =
    println(
      findRedundantConnection(
        Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(1, 4), Array(1, 5))
      ).toList
    )

  private def findRedundantConnection(edges: Array[Array[Int]]): Array[Int] =
    val dsu = new DisjointSetUnion
    edges.find { case Array(from, to) =>
      !dsu.union(from, to)
    }.get

  class DisjointSetUnion:
    private val components = mutable.Map.empty[Int, Int]
    private val parents    = mutable.Map.empty[Int, Int]
    def union(v1: Int, v2: Int): Boolean =
      val v1Component = find(v1)
      val v2Component = find(v2)
      if v1Component == v2Component then return false

      val v1ComponentRank = components.getOrElse(v1Component, 1)
      val v2ComponentRank = components.getOrElse(v2Component, 1)
      if v1ComponentRank >= v2ComponentRank then
        parents.update(v2Component, v1Component)
        components.update(v1Component, v1ComponentRank + v2ComponentRank)
        components.remove(v2Component)
      else {
        parents.update(v1Component, v2Component)
        components.update(v2Component, v2ComponentRank + v1ComponentRank)
        components.remove(v1Component)
      }
      true

    def find(v: Int): Int =
      var root = v
      while parents.contains(root) do root = parents(root)
      root
