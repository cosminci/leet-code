package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

object _106_ConstructBinaryTreeFromInOrderAndPostOrderTraversal:
  def main(args: Array[String]): Unit =
    val root = buildTree(Array(9, 3, 15, 20, 7), Array(9, 15, 7, 20, 3))
    println(root)

  def buildTree(inorder: Array[Int], postorder: Array[Int]): TreeNode =
    val inorderLookup = inorder.zipWithIndex.toMap

    def dfs(inStart: Int, inEnd: Int, postStart: Int, postEnd: Int): TreeNode =
      if (postEnd <= postStart) Option.empty.orNull
      else {
        val idx = inorderLookup(postorder(postEnd - 1))
        new TreeNode(inorder(idx),
          dfs(inStart, idx, postStart, postStart + idx - inStart),
          dfs(idx + 1, inEnd, postEnd - inEnd + idx, postEnd - 1))
      }

    dfs(inStart = 0, inEnd = inorder.length, postStart = 0, postEnd = postorder.length)
