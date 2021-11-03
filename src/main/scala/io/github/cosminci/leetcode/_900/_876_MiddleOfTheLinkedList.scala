package io.github.cosminci.leetcode._900

import io.github.cosminci.utils.ListNode

object _876_MiddleOfTheLinkedList:
  def middleNode(head: ListNode): ListNode =
    var (slow, fast) = (head, head)
    
    while fast != null && fast.next != null do
      fast = fast.next.next
      slow = slow.next
      
    slow
