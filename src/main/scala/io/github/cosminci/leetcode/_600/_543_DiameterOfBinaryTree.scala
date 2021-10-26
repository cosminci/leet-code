package io.github.cosminci.leetcode._600

import io.github.cosminci.utils.TreeNode

object _543_DiameterOfBinaryTree:

  def main(args: Array[String]): Unit =
    println(diameterOfBinaryTree(new TreeNode(1, _left = new TreeNode(2))))

  def diameterOfBinaryTree(root: TreeNode): Int =
    var globalMax = 0

    def dfs(node: TreeNode): Int =
      if node == null then 0
      else if node.left == null && node.right == null then 1
      else {
        val (leftMax, rightMax) = (dfs(node.left), dfs(node.right))
        globalMax = math.max(leftMax + rightMax, globalMax)
        1 + math.max(leftMax, rightMax)
      }

    dfs(root)
    globalMax
