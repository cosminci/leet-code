package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _138_CopyListWithRandomPointer:

  def copyRandomList(head: Node): Node =
    val oldToNew = Iterator
      .iterate(head)(_.next)
      .takeWhile(_ != null)
      .foldLeft(Map.empty[Node, Node]) { (acc, curr) =>
        acc.updated(curr, new Node(curr.value))
      }

    oldToNew.foreach { case (oldNode, newNode) =>
      newNode.next = oldToNew.getOrElse(oldNode.next, null)
      newNode.random = oldToNew.getOrElse(oldNode.random, null)
    }

    oldToNew.getOrElse(head, null)

  class Node(var _value: Int):
    var value: Int   = _value
    var next: Node   = _
    var random: Node = _
