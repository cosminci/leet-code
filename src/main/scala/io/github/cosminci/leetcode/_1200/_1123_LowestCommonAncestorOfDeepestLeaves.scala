package io.github.cosminci.leetcode._1200

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _1123_LowestCommonAncestorOfDeepestLeaves:
  def main(args: Array[String]): Unit =
    print(lcaDeepestLeaves(new TreeNode(0, new TreeNode(1, null, new TreeNode(2)), new TreeNode(3))).value)

  private def lcaDeepestLeaves(root: TreeNode): TreeNode =
    def dfs(node: TreeNode): (TreeNode, Int) =
      if node == null then return (null, 0)

      val (leftLCA, leftHeight)   = dfs(node.left)
      val (rightLCA, rightHeight) = dfs(node.right)

      if leftHeight > rightHeight then (leftLCA, leftHeight + 1)
      else if leftHeight < rightHeight then (rightLCA, rightHeight + 1)
      else (node, leftHeight + 1)

    dfs(root)._1
