package com.leetcode.cosminci._600

import com.leetcode.cosminci.utils.Node

object _559_MaxDepthOfNAryTree:
  def maxDepth(root: Node): Int =
    if root == null then return 0
    def dfs(node: Node): Int =
      if node.children.isEmpty then 1
      else 1 + node.children.map(dfs).max
    dfs(root)
