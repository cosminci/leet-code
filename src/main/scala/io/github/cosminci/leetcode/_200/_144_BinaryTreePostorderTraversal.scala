package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

object _144_BinaryTreePostorderTraversal {

  def postorderTraversal(root: TreeNode): List[Int] =
    def dfs(node: TreeNode): Seq[Int] =
      if (node == null) Seq.empty
      else dfs(node.left) ++ dfs(node.right) :+ node.value

    dfs(root).toList
}
