package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

object _106_ConstructBinaryTreeFromInOrderAndPreOrderTraversal:
  def main(args: Array[String]): Unit =
    val root = buildTree(Array(3, 9, 20, 15, 7), Array(9, 3, 15, 20, 7))
    println(root)

  def buildTree(preorder: Array[Int], inorder: Array[Int]): TreeNode =
    def dfs(preord: Array[Int], inord: Array[Int]): TreeNode =
      if preord.isEmpty || inord.isEmpty then return null
      val node = new TreeNode(preord.head)

      val leftRightDivide     = inord.indexOf(preord.head)
      val (preLeft, preRight) = preord.tail.splitAt(leftRightDivide)
      val (inLeft, inRight)   = inord.splitAt(leftRightDivide)

      node.left = dfs(preLeft, inLeft)
      node.right = dfs(preRight, inRight.tail)
      node
    dfs(preorder, inorder)
