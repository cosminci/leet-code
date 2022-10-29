package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

object _104_MaxDepthOfBinaryTree:

  def maxDepth(root: TreeNode): Int =
    if root == null then 0
    else 1 + maxDepth(root.left).max(maxDepth(root.right))
