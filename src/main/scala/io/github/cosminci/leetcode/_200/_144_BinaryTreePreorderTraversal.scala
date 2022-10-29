package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

object _144_BinaryTreePreorderTraversal {

  def preorderTraversal(root: TreeNode): List[Int] =
    def dfs(node: TreeNode): List[Int] =
      if (node == null) List.empty
      else node.value +: dfs(node.left) ++: dfs(node.right)

    dfs(root)
}
