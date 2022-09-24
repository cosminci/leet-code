package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

object _113_PathSumII:

  def pathSum(root: TreeNode, targetSum: Int): List[List[Int]] =
    def dfs(path: Seq[TreeNode], remaining: Int): Seq[Seq[TreeNode]] =
      val node = path.last
      if node.left == null && node.right == null then
        Option.when(node.value == remaining)(Seq(path)).getOrElse(Seq.empty)
      else
        Seq(Option(node.left), Option(node.right)).flatten
          .flatMap(n => dfs(path :+ n, remaining - node.value))

    if root == null then List.empty
    else dfs(Seq(root), targetSum).map(_.map(_.value).toList).toList
