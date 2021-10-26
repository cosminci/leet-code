package io.github.cosminci.leetcode._500

import io.github.cosminci.utils.Node

import scala.collection.mutable

object _429_NAryTreeLevelOrderTraversal {
  private def levelOrder(root: Node): List[List[Int]] = {
    if (root == null) return List.empty

    val result = mutable.ListBuffer.empty[List[Int]]
    val toVisit = mutable.Queue(root)
    while (toVisit.nonEmpty) {
      val levelNodes = toVisit.dequeueAll(_ => true)
      levelNodes.foreach { n =>
        n.children.foreach(toVisit.enqueue)
      }
      result.append(levelNodes.map(_.value).toList)
    }
    result.toList
  }
}
