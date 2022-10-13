package io.github.cosminci.leetcode._300

import io.github.cosminci.utils.ListNode

object _237_DeleteNodeInLinkedList:

  def deleteNode(node: ListNode): Unit =
    @annotation.tailrec
    def dfs(curr: ListNode, next: ListNode): Unit =
      curr.x = next.x
      if next.next == null then curr.next = null
      else dfs(next, next.next)

    dfs(curr = node, next = node.next)
