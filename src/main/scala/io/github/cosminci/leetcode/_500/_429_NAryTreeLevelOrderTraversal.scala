package io.github.cosminci.leetcode._500

import io.github.cosminci.utils.Node

import scala.collection.mutable

object _429_NAryTreeLevelOrderTraversal:

  def levelOrder(root: Node): List[List[Int]] =
    def dfs(curr: Seq[Node]): List[List[Int]] =
      if curr.isEmpty then List.empty
      else curr.map(_.value).toList +: dfs(curr.flatMap(_.children))

    dfs(curr = Seq(Option(root)).flatten)
