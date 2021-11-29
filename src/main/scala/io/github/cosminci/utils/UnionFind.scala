package io.github.cosminci.utils

import scala.collection.mutable

class UnionFind[T]:
  private val rank: mutable.Map[T, Int] = mutable.Map.empty.withDefaultValue(1)
  private val parent: mutable.Map[T, T] = mutable.Map.empty.withDefault(n => n)

  def union(n1: T, n2: T): T =
    val (p1, p2) = (find(n1), find(n2))
    if p1 != p2 then
      val (lo, hi) = Option.when(rank(p1) >= rank(p2))((p2, p1)).getOrElse((p1, p2))
      rank.update(hi, rank(lo) + rank(hi))
      parent.update(lo, hi)
    parent(p1)

  def find(n: T): T =
    if parent(n) != n then parent.update(n, find(parent(n)))
    parent(n)
