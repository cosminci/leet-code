package com.leetcode.cosminci._2000

import com.leetcode.cosminci.utils.UnionFind

import scala.collection.mutable

object _1971_FindIfPathExistsInGraph:

  def main(args: Array[String]): Unit =
    println(validPath(10, Array(Array(5, 3), Array(3, 6), Array(6, 9)), 5, 9))

  def validPath(n: Int, edges: Array[Array[Int]], start: Int, end: Int): Boolean =
    val uf = new UnionFind[Int]
    edges.foreach { case Array(n1, n2) => uf.union(n1, n2) }
    uf.find(start) == uf.find(end)
