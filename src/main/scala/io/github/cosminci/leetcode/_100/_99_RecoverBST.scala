package io.github.cosminci.leetcode._100

import io.github.cosminci.utils.TreeNode

object _99_RecoverBST:
  def recoverTree(root: TreeNode): Unit =
    var firstNode: TreeNode  = null
    var secondNode: TreeNode = null
    var prevNode: TreeNode   = null

    def traverseInOrder(currentNode: TreeNode): Unit =
      if currentNode == null then return ()

      traverseInOrder(currentNode.left)

      if prevNode != null && firstNode == null && prevNode.value >= currentNode.value then firstNode = prevNode
      if firstNode != null && prevNode.value >= currentNode.value then secondNode = currentNode
      prevNode = currentNode

      traverseInOrder(currentNode.right)

    traverseInOrder(root)
    val tmp = firstNode.value
    firstNode.value = secondNode.value
    secondNode.value = tmp
