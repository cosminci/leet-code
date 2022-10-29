package com.leetcode.cosminci._600

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _515_FindLargestValueInEachTreeRow {
  def largestValues(root: TreeNode): List[Int] = {
    if (root == null) return List.empty

    val results = mutable.ListBuffer.empty[Int]
    val queue = mutable.Queue(root)

    while (queue.nonEmpty) {
      val level = queue.dequeueAll(_ => true)
      results.append(level.map(_.value).max)
      level.foreach { n =>
        (Seq.empty ++ Option(n.left) ++ Option(n.right)).foreach(queue.enqueue)
      }
    }

    results.toList
  }
}
