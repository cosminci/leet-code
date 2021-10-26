package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _116_PopulatingNextRightPointersInEachNode:
  class Node(var _value: Int):
    var value: Int  = _value
    var left: Node  = null
    var right: Node = null
    var next: Node  = null

  def main(args: Array[String]): Unit =
    val root = new Node(1)
    root.left = new Node(2)
    root.right = new Node(3)
    connectLevelOrderTraversal(root)

  private def connectLevelOrderTraversal(root: Node): Node =
    if root == null then return null

    val toVisit = mutable.Queue(root)
    while toVisit.nonEmpty do
      val levelNodes = toVisit.dequeueAll(_ => true)
      levelNodes.indices.foreach { i =>
        if levelNodes(i).left != null then toVisit.enqueue(levelNodes(i).left)
        if levelNodes(i).right != null then toVisit.enqueue(levelNodes(i).right)
        if i < levelNodes.length - 1 then levelNodes(i).next = levelNodes(i + 1)
      }

    root

  private def connectIterative(root: Node): Node =
    var leftmost = root
    while leftmost != null && leftmost.left != null do
      var curr = leftmost
      while curr != null do
        curr.left.next = curr.right
        if curr.next != null then curr.right.next = curr.next.left
        curr = curr.next
      leftmost = leftmost.left
    return root
