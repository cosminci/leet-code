package com.leetcode.cosminci._700

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _653_TwoSumIVInputBST:

  def findTarget(root: TreeNode, k: Int): Boolean =
    @annotation.tailrec
    def dfs(prev: Set[Int], next: Seq[TreeNode]): Boolean =
      next.headOption match
        case None => false
        case Some(curr) =>
          prev.contains(k - curr.value) ||
            dfs(prev + curr.value, next.tail ++ Option(curr.left) ++ Option(curr.right))

    dfs(prev = Set.empty, next = Seq(root))
