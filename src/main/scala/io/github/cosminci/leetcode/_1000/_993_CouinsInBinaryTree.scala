package io.github.cosminci.leetcode._1000

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _993_CouinsInBinaryTree:

  def isCousins(root: TreeNode, x: Int, y: Int): Boolean =
    def dfs(node: TreeNode, parent: Int, depth: Int): Map[Int, (Int, Int)] =
      if Option(node).isEmpty then Map.empty
      else
        Map((node.value -> (parent, depth))) ++
          dfs(node.left, node.value, depth + 1) ++
          dfs(node.right, node.value, depth + 1)

    val nodeInfo = dfs(root, parent = -1, depth = 0)

    val (xParent, xDepth) = nodeInfo(x)
    val (yParent, yDepth) = nodeInfo(y)

    xParent != yParent && xDepth == yDepth
