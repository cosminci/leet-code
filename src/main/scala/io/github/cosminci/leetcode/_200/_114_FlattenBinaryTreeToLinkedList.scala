package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _114_FlattenBinaryTreeToLinkedList:
  def flatten(root: TreeNode): Unit =
    if root == null then return

    val dummyHead = new TreeNode(0, null, null)
    var curr      = dummyHead
    val stack     = mutable.Stack(root)

    while stack.nonEmpty do
      val node = stack.pop()
      curr.right = node
      curr = node

      if node.right != null then stack.push(node.right)
      if node.left != null then stack.push(node.left)

      curr.left = null
