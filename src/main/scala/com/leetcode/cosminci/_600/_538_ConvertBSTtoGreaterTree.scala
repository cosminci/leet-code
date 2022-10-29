package com.leetcode.cosminci._600

import com.leetcode.cosminci.utils.TreeNode

object _538_ConvertBSTtoGreaterTree:

  def convertBST(root: TreeNode): TreeNode =
    def dfs(node: TreeNode, sum: Int): Int =
      if node == null then sum
      else
        node.value += dfs(node.right, sum)
        dfs(node.left, node.value)

    dfs(node = root, sum = 0)
    root
