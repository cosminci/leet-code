package com.leetcode.cosminci._1400

import com.leetcode.cosminci.utils.TreeNode

object _1372_LongestZigZagPathInBinaryTree:

  def longestZigZag(root: TreeNode): Int =
    def dfs(node: TreeNode, depth: Int, dir: Boolean): Int =
      if node == null then depth
      else if dir then dfs(node.right, depth + 1, !dir).max(dfs(node.left, depth = 1, dir))
      else dfs(node.left, depth + 1, !dir).max(dfs(node.right, depth = 1, dir))

    dfs(node = root, depth = 0, dir = true).max(dfs(node = root, depth = 0, dir = false))
