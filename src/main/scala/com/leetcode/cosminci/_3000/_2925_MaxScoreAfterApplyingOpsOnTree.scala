package com.leetcode.cosminci._3000

import scala.util.chaining.*

object _2925_MaxScoreAfterApplyingOpsOnTree:

  def maximumScoreAfterOperations(edges: Array[Array[Int]], values: Array[Int]): Long =
    val tree = edges.foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) { (tree, edge) =>
      tree
        .updated(edge(0), tree(edge(0)) :+ edge(1))
        .updated(edge(1), tree(edge(1)) :+ edge(0))
    }

    def dfs(curr: Int, parent: Int): (Long, Long) =
      tree(curr)
        .filterNot(_ == parent)
        .foldLeft(0L, 0L) { case ((leftout, taken), next) =>
          dfs(next, curr).pipe { case (l, t) => (leftout + l, taken + t) }
        }
        .pipe { case (l, t) =>
          val taken   = if l == 0 then t else t + l.max(values(curr))
          val leftout = if l == 0 then values(curr).toLong else l.min(values(curr))
          (leftout, taken)
        }

    dfs(curr = 0, parent = -1).pipe { case (_, taken) => taken }
