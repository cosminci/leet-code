package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _138_CopyListWithRandomPointer:
  def main(args: Array[String]): Unit = {}

  def copyRandomListDoublePass(head: Node): Node =
    if head == null then return null
    val oldToNew = mutable.Map.empty[Node, Node]
    var curr     = head
    while curr != null do
      val newCurr = new Node(curr.value)
      oldToNew.update(curr, newCurr)
      curr = curr.next

    curr = head
    while curr != null do
      val newCurr = oldToNew(curr)
      newCurr.next = oldToNew.getOrElse(curr.next, null)
      newCurr.random = oldToNew.getOrElse(curr.random, null)
      curr = curr.next

    oldToNew(head)

  def copyRandomListSinglePass(head: Node): Node =
    if head == null then return null

    val newHead  = new Node(head.value)
    val oldToNew = mutable.Map(head -> newHead)
    val missingLinks =
      if head.random != head then mutable.Map(head.random -> mutable.ListBuffer(newHead))
      else {
        newHead.random = newHead
        mutable.Map.empty[Node, mutable.ListBuffer[Node]]
      }

    var (prev, curr) = (newHead, head.next)
    while curr != null do
      val newNode = new Node(curr.value)
      oldToNew.update(curr, newNode)
      prev.next = newNode

      if missingLinks.contains(curr) then
        missingLinks(curr).foreach { n =>
          n.random = newNode
        }

      if oldToNew.contains(curr.random) then newNode.random = oldToNew(curr.random)
      else missingLinks.getOrElseUpdate(curr.random, mutable.ListBuffer.empty).addOne(newNode)

      prev = newNode
      curr = curr.next

    newHead

  class Node(var _value: Int):
    var value: Int   = _value
    var next: Node   = null
    var random: Node = null
