package com.leetcode.cosminci._100

import com.leetcode.cosminci.utils.ListNode

object _83_RemoveDuplicatesFromSortedList:
  def deleteDuplicates(head: ListNode): ListNode =
    val dummy        = new ListNode(-101, head)
    var (prev, curr) = (dummy, head)

    while curr != null do
      if prev.x == curr.x then prev.next = curr.next
      else prev = curr
      curr = curr.next

    dummy.next
