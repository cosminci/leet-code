package io.github.cosminci.leetcode._500

object _430_FlattenMultiLevelDoublyLinkedList:
  def main(args: Array[String]): Unit =
    val head = new Node(1)
    head.child = new Node(2)
    head.child.child = new Node(3)
    val result = flatten(head)
    println(result)

  class Node(var _value: Int):
    var value: Int  = _value
    var prev: Node  = null
    var next: Node  = null
    var child: Node = null

  private def flatten(head: Node): Node =
    if head == null then return null

    var curr = head
    while curr != null do
      if curr.child == null then curr = curr.next
      else
        var branchCurr = curr.child
        while branchCurr.next != null do branchCurr = branchCurr.next

        branchCurr.next = curr.next
        if curr.next != null then curr.next.prev = branchCurr

        curr.next = curr.child
        curr.child.prev = curr
        curr.child = null

    head
