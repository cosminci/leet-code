package io.github.cosminci.leetcode._600

import io.github.cosminci.utils.TreeNode

object _538_ConvertBSTtoGreaterTree:

  private def convertBST(root: TreeNode): TreeNode =
    def dfs(node: TreeNode, sum: Int): Int =
      if node == null then sum
      else
        node.value += dfs(node.right, sum)
        dfs(node.left, node.value)

    dfs(node = root, sum = 0)
    root
