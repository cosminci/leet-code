package io.github.cosminci.leetcode._2100

import io.github.cosminci.utils.TreeNode

object _2096_StepByStepDirectionsFromABinaryTreeNodeToAnother:
  def main(args: Array[String]): Unit =
    println(getDirections(new TreeNode(2, new TreeNode(1)), 2, 1))

  def getDirections(root: TreeNode, startValue: Int, destValue: Int): String =
    def dfs(node: TreeNode, target: Int): Option[String] =
      if node == null then None
      else if node.value == target then Some("")
      else dfs(node.left, target).map('L' +: _).orElse(dfs(node.right, target).map('R' +: _))

    val Some(pathToStart) = dfs(root, startValue)
    val Some(pathToDest)  = dfs(root, destValue)

    val commonPrefixLength = pathToStart.zip(pathToDest).takeWhile { case (c1, c2) => c1 == c2 }.length
    "U" * (pathToStart.length - commonPrefixLength) ++ pathToDest.drop(commonPrefixLength)
