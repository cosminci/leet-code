package com.leetcode.cosminci._200

import scala.collection.mutable

object _117_PopulatingNextRightPointersInEachNodeII:

  def connect(root: Node): Node =
    var node = root
    while node != null do
      val dummy = new Node(0)
      var curr  = dummy
      while node != null do
        if node.left != null then
          curr.next = node.left
          curr = curr.next
        if node.right != null then
          curr.next = node.right
          curr = curr.next
        node = node.next
      node = dummy.next
    root

  class Node(var _value: Int):
    var value: Int  = _value
    var left: Node  = null
    var right: Node = null
    var next: Node  = null
