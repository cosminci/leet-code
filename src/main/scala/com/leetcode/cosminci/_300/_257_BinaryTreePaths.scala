package com.leetcode.cosminci._300

import com.leetcode.cosminci.utils.TreeNode

object _257_BinaryTreePaths:

  def binaryTreePaths(root: TreeNode): List[String] =
    def dfs(node: TreeNode): List[String] =
      if node == null then List.empty
      else
        val below = dfs(node.left) ++ dfs(node.right)
        if below.isEmpty then List(node.value.toString)
        else below.map(tail => s"${node.value}->$tail")

    dfs(root)
