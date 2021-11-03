package io.github.cosminci.leetcode._300

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _226_InvertBinaryTree:
  def invertTreeRecursive(root: TreeNode): TreeNode =
    if root == null then return null
    val tmp = root.left
    root.left = invertTreeRecursive(root.right)
    root.right = invertTreeRecursive(tmp)
    root

  def invertTreeQueue(root: TreeNode): TreeNode =
    if root == null then return null
    val queue = mutable.Queue(root)

    while queue.nonEmpty do
      val node = queue.dequeue()
      if node.left != null then queue.enqueue(node.left)
      if node.right != null then queue.enqueue(node.right)

      val tmp = node.left
      node.left = node.right
      node.right = tmp
    root
