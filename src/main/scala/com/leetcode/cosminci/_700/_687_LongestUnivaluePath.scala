package com.leetcode.cosminci._700

import com.leetcode.cosminci.utils.TreeNode

object _687_LongestUnivaluePath:
  def main(args: Array[String]): Unit =
    println(longestUnivaluePath(new TreeNode(1)))
    println(
      longestUnivaluePath(
        new TreeNode(1, new TreeNode(4, new TreeNode(4), new TreeNode(4)), new TreeNode(5, null, new TreeNode(5)))
      )
    )

  def longestUnivaluePath(root: TreeNode): Int =
    var max = 0

    def dfs(node: TreeNode): (Int, Int) =
      if node == null then return (0, 1001)
      val (leftMax, leftValue)   = dfs(node.left)
      val (rightMax, rightValue) = dfs(node.right)

      if leftValue == node.value && rightValue == node.value then
        max = math.max(max, 2 + leftMax + rightMax)
        (1 + math.max(leftMax, rightMax), node.value)
      else if leftValue == node.value then
        max = math.max(max, 1 + leftMax)
        (1 + leftMax, node.value)
      else if rightValue == node.value then
        max = math.max(max, 1 + rightMax)
        (1 + rightMax, node.value)
      else (0, node.value)

    dfs(root)
    max
