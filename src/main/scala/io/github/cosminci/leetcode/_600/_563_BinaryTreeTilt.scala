package io.github.cosminci.leetcode._600

import io.github.cosminci.utils.TreeNode

object _563_BinaryTreeTilt:
  def findTilt(root: TreeNode): Int =
    def dfs(node: TreeNode): (Int, Int) =
      if node == null then (0, 0)
      else
        val (leftTilt, leftSum)   = dfs(node.left)
        val (rightTilt, rightSum) = dfs(node.right)
        (leftTilt + rightTilt + math.abs(rightSum - leftSum), leftSum + rightSum + node.value)

    dfs(root)._1
