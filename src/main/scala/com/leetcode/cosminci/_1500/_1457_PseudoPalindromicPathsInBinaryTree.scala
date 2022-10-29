package com.leetcode.cosminci._1500

import com.leetcode.cosminci.utils.TreeNode

object _1457_PseudoPalindromicPathsInBinaryTree:

  def pseudoPalindromicPaths(root: TreeNode): Int =
    def dfs(node: TreeNode, bitmask: Int): Int =
      val newBitmask = 1 << node.value ^ bitmask

      if node.left == null && node.right == null then
        if (1 to 9).count(shift => (newBitmask >> shift & 1) > 0) <= 1 then 1 else 0
      else
        Seq(Option(node.left), Option(node.right)).flatten.map(dfs(_, newBitmask)).sum

    dfs(root, bitmask = 0)
