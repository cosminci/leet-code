package io.github.cosminci.leetcode._100

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _94_BinaryTreeInOrderTraversal:

  def inorderTraversalRecursive(root: TreeNode): List[Int] =
    def dfs(node: TreeNode): List[Int] =
      if node == null then List.empty
      else (dfs(node.left) :+ node.value) ++ dfs(node.right)
    dfs(root)
