package io.github.cosminci.leetcode._2000

import io.github.cosminci.utils.DisjointSetUnion.DSU

import scala.collection.mutable

object _1971_FindIfPathExistsInGraph:

  def main(args: Array[String]): Unit =
    println(validPath(10, Array(Array(5, 3), Array(3, 6), Array(6, 9)), 5, 9))

  private def validPath(n: Int, edges: Array[Array[Int]], start: Int, end: Int): Boolean =
    val dsu = new DSU
    edges.foreach { case Array(n1, n2) => dsu.union(n1, n2) }
    dsu.find(start) == dsu.find(end)
