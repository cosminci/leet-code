package io.github.cosminci.leetcode._1000

import io.github.cosminci.utils.TreeNode

object _951_FlipEquivalentBinaryTrees:
  def main(args: Array[String]): Unit =
    println(flipEquiv(new TreeNode(2), new TreeNode(2)))

  private def flipEquiv(root1: TreeNode, root2: TreeNode): Boolean =
    def dfs(node1: TreeNode, node2: TreeNode): Boolean =
      if node1 == null && node2 == null then return true
      if node1 == null || node2 == null || node1.value != node2.value then return false

      val node1Left  = Option(node1.left).map(_.value)
      val node2Left  = Option(node2.left).map(_.value)
      val node1Right = Option(node1.right).map(_.value)
      val node2Right = Option(node2.right).map(_.value)

      if node1Left == node2Left && node1Right == node2Right then
        dfs(node1.left, node2.left) && dfs(node1.right, node2.right)
      else if node1Left == node2Right && node1Right == node2Left then
        dfs(node1.left, node2.right) && dfs(node1.right, node2.left)
      else false

    dfs(root1, root2)
