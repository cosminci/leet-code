package com.leetcode.cosminci._1100

import scala.collection.mutable

object _1061_LexicographicallySmallestEquivString:

  def smallestEquivalentString(s1: String, s2: String, baseStr: String): String =
    val uf = new UnionFind
    s1.zip(s2).foreach { case (c1, c2) => uf.union(c1, c2) }
    baseStr.map(uf.find)

  class UnionFind:
    private val parents: mutable.Map[Char, Char] = mutable.Map.empty.withDefault(n => n)

    def union(c1: Char, c2: Char): Unit =
      val (p1, p2) = (find(c1), find(c2))
      if p1 != p2 then
        parents.update(p1 max p2, p1 min p2)

    def find(c: Char): Char =
      if parents(c) == c then c else find(parents(c))
