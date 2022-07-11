package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _199_BinaryTreeRightSideView:

  def rightSideView(root: TreeNode): List[Int] =
    val results = mutable.ListBuffer.empty[Int]

    def dfs(node: TreeNode, level: Int): Unit =
      if (node != null) {
        if (level >= results.length) results.append(node.value)
        dfs(node.right, level + 1)
        dfs(node.left, level + 1)
      }

    dfs(root, 0)
    results.toList
