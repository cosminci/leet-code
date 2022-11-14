package com.leetcode.cosminci._2500

import com.leetcode.cosminci.utils.TreeNode

import scala.util.chaining.*

object _2471_MinOpsToSortBinaryTreeByLevel:

  def minimumOperations(root: TreeNode): Int =
    def sort(nodes: Array[Int]): Int =
      val pos = nodes.sorted.zipWithIndex.toMap
      nodes.indices
        .foldLeft(Set.empty[Int], 0) { case ((vis, totalCnt), i) =>
          Iterator
            .iterate((vis, i, 0)) { case (vis, i, cnt) => (vis + i, pos(nodes(i)), cnt + 1) }
            .dropWhile { case (vis, i, _) => !vis.contains(i) && i != pos(nodes(i)) }
            .next()
            .pipe { case (vis, _, localCnt) => (vis, totalCnt + 0.max(localCnt - 1)) }
        }
        .pipe { case (_, totalCnt) => totalCnt }

    def nextLevel(level: Array[TreeNode]) = level.flatMap(n => Seq(Option(n.left), Option(n.right)).flatten)
    Iterator
      .iterate((Array(root), 0)) { case (level, cnt) => (nextLevel(level), sort(level.map(_.value))) }
      .dropWhile { case (level, _) => level.nonEmpty }
      .next()
      .pipe { case (_, totalCnt) => totalCnt }
