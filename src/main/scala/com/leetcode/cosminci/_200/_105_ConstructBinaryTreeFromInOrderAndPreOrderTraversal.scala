package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

object _105_ConstructBinaryTreeFromInOrderAndPreOrderTraversal:
  def main(args: Array[String]): Unit =
    val root = buildTree(Array(3, 9, 20, 15, 7), Array(9, 3, 15, 20, 7))
    println(root)

  def buildTree(preorder: Array[Int], inorder: Array[Int]): TreeNode =
    def dfs(preord: Array[Int], inord: Array[Int]): TreeNode =
      if preord.isEmpty || inord.isEmpty then Option.empty.orNull
      else {
        val leftRightDivide     = inord.indexOf(preord.head)
        val (preLeft, preRight) = preord.tail.splitAt(leftRightDivide)
        val (inLeft, inRight)   = inord.splitAt(leftRightDivide)
        new TreeNode(preord.head, dfs(preLeft, inLeft), dfs(preRight, inRight.tail))
      }
    dfs(preorder, inorder)
