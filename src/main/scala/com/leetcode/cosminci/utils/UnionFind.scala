package com.leetcode.cosminci.utils

import scala.collection.mutable

class UnionFind[T]:
  private val ranks: mutable.Map[T, Int] = mutable.Map.empty.withDefaultValue(1)
  private val parents: mutable.Map[T, T] = mutable.Map.empty.withDefault(n => n)

  def union(n1: T, n2: T): T =
    val (p1, p2) = (find(n1), find(n2))
    if p1 != p2 then
      val (lo, hi) = Option.when(ranks(p1) >= ranks(p2))((p2, p1)).getOrElse((p1, p2))
      ranks.update(hi, ranks(lo) + ranks(hi))
      parents.update(lo, hi)
    parents(p1)

  def find(n: T): T =
    if parents(n) != n then parents.update(n, find(parents(n)))
    parents(n)

  def nodes(): Set[T] = parents.keySet.toSet
  def rank(n: T): Int = ranks(n)
