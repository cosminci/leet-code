package io.github.cosminci.leetcode._700

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _637_AverageOfLevelsInBinaryTree:

  def averageOfLevels(root: TreeNode): Array[Double] =
    @annotation.tailrec
    def dfs(level: Seq[TreeNode], res: Array[Double]): Array[Double] =
      if level.isEmpty then res
      else
        dfs(
          level.flatMap(node => Seq(Option(node.left), Option(node.right)).flatten),
          res :+ level.map(_.value.toDouble).sum / level.length
        )

    dfs(level = Seq(root), res = Array.empty)
