package com.leetcode.cosminci._100

import com.leetcode.cosminci.utils.TreeNode

object _95_UniqueBSTsII:
  def main(args: Array[String]): Unit =
    val forest = generateTrees(3)
    println(forest)

  def generateTrees(n: Int): List[TreeNode] =
    def dfs(start: Int, end: Int): Seq[TreeNode] =
      (start to end).flatMap { rootValue =>
        for
          leftChild  <- if rootValue == start then Seq(null) else dfs(start, rootValue - 1)
          rightChild <- if rootValue == end then Seq(null) else dfs(rootValue + 1, end)
        yield new TreeNode(rootValue, leftChild, rightChild)
      }
    dfs(1, n).toList
