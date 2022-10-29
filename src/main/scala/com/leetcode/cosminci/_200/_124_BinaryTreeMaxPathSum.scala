package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

object _124_BinaryTreeMaxPathSum:

  def maxPathSum(root: TreeNode): Int =
    var max = Int.MinValue
    def dfs(node: TreeNode): Int =
      if node == null then return 0

      val leftMax  = math.max(0, dfs(node.left))
      val rightMax = math.max(0, dfs(node.right))
      val localMax = leftMax + rightMax + node.value
      if localMax > max then max = localMax

      math.max(leftMax, rightMax) + node.value
    dfs(root)
    max
