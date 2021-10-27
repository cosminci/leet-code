package io.github.cosminci.utils

import scala.collection.mutable

object DisjointSetUnion:

  class DSU:
    private val rank: mutable.Map[Int, Int]   = mutable.Map.empty.withDefaultValue(1)
    private val parent: mutable.Map[Int, Int] = mutable.Map.empty.withDefault(n => n)

    def union(n1: Int, n2: Int): Unit =
      val (p1, p2) = (find(n1), find(n2))
      if p1 != p2 then
        if rank(p1) >= rank(p2) then
          rank.update(p1, rank(p1) + rank(p2))
          parent.update(p2, p1)
        else
          rank.update(p2, rank(p1) + rank(p2))
          parent.update(p1, p2)

    def find(n: Int): Int =
      if parent(n) != n then parent.update(n, find(parent(n)))
      parent(n)
