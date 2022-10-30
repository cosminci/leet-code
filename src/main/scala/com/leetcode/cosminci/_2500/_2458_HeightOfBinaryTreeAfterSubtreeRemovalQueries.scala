package com.leetcode.cosminci._2500

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _2458_HeightOfBinaryTreeAfterSubtreeRemovalQueries:

  def treeQueries(root: TreeNode, queries: Array[Int]): Array[Int] =
    val heights = mutable.Map.empty[Int, Int]
    val depths  = mutable.Map.empty[Int, Int]

    def dfs(node: TreeNode, depth: Int): Int =
      if node == null then -1
      else
        depths.update(node.value, depth)
        heights.update(node.value, Seq(node.left, node.right).map(dfs(_, depth + 1)).max + 1)
        heights(node.value)

    dfs(root, depth = 0)

    val cousins = depths.keys.toSeq
      .groupMap(depths)(value => (heights(value), value))
      .map { case (depth, heights) =>
        (depth, heights.sortBy { case (height, value) => (-height, value) }.take(2))
      }

    queries.map { value =>
      cousins(depths(value)) match
        case Seq((h, v))                          => depths(value) - 1
        case Seq((_, v1), (h2, _)) if v1 == value => h2 + depths(value)
        case Seq((h1, _), _)                      => h1 + depths(value)
    }
