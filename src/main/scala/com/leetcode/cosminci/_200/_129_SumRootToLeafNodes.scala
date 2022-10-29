package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

object _129_SumRootToLeafNodes:
  def main(args: Array[String]): Unit =
    println(sumNumbers(new TreeNode(1, new TreeNode(2), new TreeNode(3))))

  def sumNumbers(root: TreeNode): Int =
    def dfs(prev: Int, node: TreeNode): Int =
      if node == null then return 0

      val newValue = prev * 10 + node.value

      if node.left == null && node.right == null then newValue
      else dfs(newValue, node.left) + dfs(newValue, node.right)

    dfs(0, root)
