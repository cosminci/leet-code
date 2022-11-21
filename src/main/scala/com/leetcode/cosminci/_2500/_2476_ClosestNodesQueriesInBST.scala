package com.leetcode.cosminci._2500

import com.leetcode.cosminci.utils.TreeNode

object _2476_ClosestNodesQueriesInBST:

  def closestNodes(root: TreeNode, queries: List[Int]): List[List[Int]] =
    @annotation.tailrec
    def dfs(node: TreeNode, v: Int, lower: Int, higher: Int): List[Int] =
      if node == null then List(lower, higher)
      else if node.value == v then List(v, v)
      else if node.value > v then dfs(node.left, v, lower, node.value)
      else dfs(node.right, v, node.value, higher)

    queries.map(v => dfs(root, v, lower = -1, higher = -1))
