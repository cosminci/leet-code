package io.github.cosminci.leetcode._300

import io.github.cosminci.utils.TreeNode

object _236_LowestCommonAncestor:

  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode =
    def dfs(node: TreeNode): TreeNode =
      if node == null || node == p || node == q then node
      else
        val Seq(left, right) = Seq(node.left, node.right).map(dfs)
        if left == null then right
        else if right == null then left
        else node
    dfs(root)
