package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

object _104_MaxDepthOfBinaryTree:

  def maxDepth(root: TreeNode): Int =
    if root == null then 0
    else 1 + maxDepth(root.left).max(maxDepth(root.right))
