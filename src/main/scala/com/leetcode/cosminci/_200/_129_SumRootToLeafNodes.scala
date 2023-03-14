package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

object _129_SumRootToLeafNodes:

  def sumNumbers(root: TreeNode): Int =
    def dfs(prev: Int, node: TreeNode): Int =
      if node == null then 0
      else {
        val curr = prev * 10 + node.value
        if node.left == null && node.right == null then curr
        else dfs(curr, node.left) + dfs(curr, node.right)
      }
    dfs(prev = 0, node = root)
