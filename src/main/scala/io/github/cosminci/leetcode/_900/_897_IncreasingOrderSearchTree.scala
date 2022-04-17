package io.github.cosminci.leetcode._900

import io.github.cosminci.utils.TreeNode

object _897_IncreasingOrderSearchTree:

  def increasingBST(root: TreeNode): TreeNode =
    def dfs(curr: TreeNode, next: TreeNode): TreeNode =
      if curr == null then next
      else
        val node = dfs(curr.left, curr)
        curr.left = null
        curr.right = dfs(curr.right, next)
        node

    dfs(curr = root, next = null)
