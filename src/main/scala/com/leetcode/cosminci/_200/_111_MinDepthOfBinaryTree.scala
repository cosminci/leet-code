package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

object _111_MinDepthOfBinaryTree:

  def minDepth(root: TreeNode): Int =
    def dfs(node: TreeNode, level: Int): Int =
      if node == null then level
      else if node.left == null && node.right == null then level + 1
      else Seq(node.left, node.right).filter(_ != null).map(dfs(_, level + 1)).min

    dfs(root, level = 0)
