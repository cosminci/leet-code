package io.github.cosminci.leetcode._1500

import io.github.cosminci.utils.TreeNode

object _1448_CountGoodNodesInBinaryTree:

  def goodNodes(root: TreeNode): Int =
    @annotation.tailrec
    def dfs(toVisit: Seq[(TreeNode, Int)], cnt: Int): Int =
      toVisit.headOption match
        case None => cnt
        case Some((node, prevMax)) =>
          val next = Seq(node.left, node.right).flatMap(Option.apply).map((_, prevMax.max(node.value)))
          dfs(toVisit.tail ++ next, Option.when(node.value >= prevMax)(1).getOrElse(0) + cnt)

    dfs(toVisit = Seq((root, root.value)), cnt = 0)
