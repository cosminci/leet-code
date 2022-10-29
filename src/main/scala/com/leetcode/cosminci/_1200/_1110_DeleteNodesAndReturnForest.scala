package com.leetcode.cosminci._1200

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _1110_DeleteNodesAndReturnForest:
  def main(args: Array[String]): Unit =
    val forest = delNodes(new TreeNode(1, new TreeNode(2), new TreeNode(3)), Array(1))
    println(forest)

  def delNodes(root: TreeNode, toDelete: Array[Int]): List[TreeNode] =
    val remainingToDelete = mutable.Set.from(toDelete)
    val roots             = mutable.ListBuffer.empty[TreeNode]

    def dfs(node: TreeNode, hasParent: Boolean): TreeNode =
      if node == null then return null

      val deleted = remainingToDelete.remove(node.value)
      if !deleted && !hasParent then roots.append(node)

      node.left = dfs(node.left, !deleted)
      node.right = dfs(node.right, !deleted)

      Option.when(!deleted)(node).orNull

    dfs(root, hasParent = false)
    roots.toList
