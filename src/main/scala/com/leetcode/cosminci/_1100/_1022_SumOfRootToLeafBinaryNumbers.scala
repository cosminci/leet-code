package com.leetcode.cosminci._1100

import com.leetcode.cosminci.utils.TreeNode

object _1022_SumOfRootToLeafBinaryNumbers:

  def sumRootToLeaf(root: TreeNode): Int =
    def dfs(node: TreeNode, v: Int): Int =
      if node.left == null && node.right == null then v * 2 + node.value
      else Seq(Option(node.left), Option(node.right)).flatten.map(dfs(_, v * 2 + node.value)).sum

    dfs(root, v = 0)
