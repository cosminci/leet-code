package io.github.cosminci.leetcode._900

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _863_AllNodesDistanceKInBinaryTree:
  def main(args: Array[String]): Unit =
    val target = new TreeNode(2)
    println(distanceK(new TreeNode(0, new TreeNode(1, new TreeNode(3), target)), target, 1))

  private def distanceK(root: TreeNode, target: TreeNode, k: Int): List[Int] =
    val parents = populateParents(root)
    val toVisit = mutable.Queue(target)
    val visited = mutable.Set(target)

    var distance = 0
    while distance != k do
      val levelNodes = toVisit.dequeueAll(_ => true)

      levelNodes.foreach { n =>
        if n.left != null && !visited.contains(n.left) then
          visited.add(n.left)
          toVisit.enqueue(n.left)
        if n.right != null && !visited.contains(n.right) then
          visited.add(n.right)
          toVisit.enqueue(n.right)
        if parents.get(n).exists(p => !visited.contains(p)) then
          visited.add(parents(n))
          toVisit.enqueue(parents(n))
      }
      distance += 1

    toVisit.dequeueAll(_ => true).map(_.value).toList

  private def populateParents(root: TreeNode) =
    val parents = mutable.Map.empty[TreeNode, TreeNode]
    val toVisit = mutable.Stack(root)

    while toVisit.nonEmpty do
      val node = toVisit.pop()
      if node.left != null then
        parents.update(node.left, node)
        toVisit.push(node.left)
      if node.right != null then
        parents.update(node.right, node)
        toVisit.push(node.right)

    parents.toMap
