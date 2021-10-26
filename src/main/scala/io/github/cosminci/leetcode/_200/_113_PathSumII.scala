package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

object _113_PathSumII:
  def main(args: Array[String]): Unit =
    println(pathSum(new TreeNode(5), 5))

  def pathSum(root: TreeNode, targetSum: Int): List[List[Int]] =
    if root == null then return List.empty

    def dfs(path: Seq[TreeNode], remaining: Int): Seq[Seq[TreeNode]] =
      val node = path.last
      if node.left == null && node.right == null then
        if node.value == remaining then return Seq(path)
        else return Seq.empty

      val left  = if node.left != null then dfs(path :+ node.left, remaining - node.value) else Seq.empty
      val right = if node.right != null then dfs(path :+ node.right, remaining - node.value) else Seq.empty
      left ++ right

    dfs(Seq(root), targetSum).map(_.map(_.value).toList).toList
