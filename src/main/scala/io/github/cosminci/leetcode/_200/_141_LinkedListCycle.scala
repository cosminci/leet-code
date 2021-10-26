package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.ListNode

object _141_LinkedListCycle:

  def hasCycle(head: ListNode): Boolean =
    if head == null || head.next == null then return false
    var slow = head
    var fast = head.next

    while fast != null && fast.next != null do
      if fast == slow then return true
      slow = slow.next
      fast = fast.next.next
    false
