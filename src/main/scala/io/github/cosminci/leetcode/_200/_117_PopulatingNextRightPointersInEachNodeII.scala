package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _117_PopulatingNextRightPointersInEachNodeII:

  class Node(var _value: Int):
    var value: Int  = _value
    var left: Node  = null
    var right: Node = null
    var next: Node  = null

  def connect(root: Node): Node =
    if root == null then return null

    var levelHead = new Node(0)
    var node      = root

    while node != null do
      var needle = levelHead

      while node != null do
        if node.left != null then
          needle.next = node.left
          needle = needle.next
        if node.right != null then
          needle.next = node.right
          needle = needle.next
        node = node.next

      node = levelHead.next
      levelHead.next = null
      
    root
