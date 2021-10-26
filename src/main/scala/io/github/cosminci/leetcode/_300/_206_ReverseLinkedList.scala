package io.github.cosminci.leetcode._300

import io.github.cosminci.utils.ListNode

object _206_ReverseLinkedList:
  private def reverseListRecursive(head: ListNode): ListNode =
    def reverse(curr: ListNode, prev: ListNode): ListNode =
      if curr == null then return prev

      val last = reverse(curr.next, curr)
      curr.next = prev
      last

    reverse(head, null)

  private def reverseListIterative(head: ListNode): ListNode =
    var (curr: ListNode, prev: ListNode) = (head, null)

    while curr != null do
      val next = curr.next
      curr.next = prev
      prev = curr
      curr = next

    return prev
