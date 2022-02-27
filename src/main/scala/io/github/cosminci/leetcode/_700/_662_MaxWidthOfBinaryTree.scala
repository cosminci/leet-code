package io.github.cosminci.leetcode._700

import io.github.cosminci.utils.TreeNode

object _662_MaxWidthOfBinaryTree:
  def widthOfBinaryTree(root: TreeNode): Int =
    def dfs(nodes: Seq[(TreeNode, Int)]): Int =
      if nodes.isEmpty then 0
      else (nodes.last._2 - nodes.head._2 + 1).max(
        dfs(
          nodes.flatMap { case (node, idx) =>
            Seq(
              Option(node.left).map(n => (n, idx * 2)),
              Option(node.right).map(n => (n, idx * 2 + 1))
            ).flatten
          }
        )
      )

    dfs(Seq((root, 0)))
