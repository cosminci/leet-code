package com.leetcode.cosminci._300

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _230_KthSmallestElementInBST:

  def kthSmallest(root: TreeNode, k: Int): Int =
    @annotation.tailrec
    def pushLeft(stack: Array[TreeNode], node: TreeNode): Array[TreeNode] =
      if node == null then stack else pushLeft(stack :+ node, node.left)

    @annotation.tailrec
    def dfs(stack: Array[TreeNode], k: Int): Int =
      if k == 1 then stack.last.value
      else dfs(pushLeft(stack.dropRight(1), stack.last.right), k - 1)

    dfs(pushLeft(Array.empty, root), k)
