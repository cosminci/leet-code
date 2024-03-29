package com.leetcode.cosminci._1000

import com.leetcode.cosminci.utils.UnionFind

object _990_SatisfiabilityOfEqualityEquations:

  def equationsPossible(equations: Array[String]): Boolean =
    val (eq, nonEq) = equations.partition(_.charAt(1) == '=')

    val uf = new UnionFind[Char]
    eq.foreach(s => uf.union(s.head, s.last))

    nonEq.forall(s => uf.find(s.head) != uf.find(s.last))
