package io.github.cosminci.utils

import scala.collection.mutable

object DisjointSetUnion:

  class DSU:
    private val rank: mutable.Map[Int, Int]   = mutable.Map.empty
    private val parent: mutable.Map[Int, Int] = mutable.Map.empty

    def union(n1: Int, n2: Int): Unit =
      val (p1, p2) = (find(n1), find(n2))
      if p1 == p2 then return

      val (p1score, p2score) = (rank(p1), rank(p2))

      if p1score >= p2score then
        rank.update(p1, p1score + p2score)
        rank.remove(p2)
        parent.update(p2, p1)
      else
        rank.update(p2, p1score + p2score)
        rank.remove(p1)
        parent.update(p1, p2)

    def find(n: Int): Int =
      if parent.get(n).exists(_ != n) then parent.update(n, find(parent(n)))
      parent(n)

    def add(n: Int): Unit =
      parent.update(n, n)
      rank.update(n, 1)
