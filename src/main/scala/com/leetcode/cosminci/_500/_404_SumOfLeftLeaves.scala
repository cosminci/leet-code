package com.leetcode.cosminci._500

import com.leetcode.cosminci.utils.TreeNode

object _404_SumOfLeftLeaves:
  def main(args: Array[String]): Unit =
    println(sumOfLeftLeaves(new TreeNode(3, new TreeNode(9))))

  def sumOfLeftLeaves(root: TreeNode): Int =
    def dfs(node: TreeNode, isLeft: Boolean): Int =
      if node == null then 0
      else if node.left == null && node.right == null & isLeft then node.value
      else dfs(node.left, isLeft = true) + dfs(node.right, isLeft = false)

    dfs(node = root, isLeft = false)
