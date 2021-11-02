package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.ListNode

object _142_LinkedListCycleII:
  def main(args: Array[String]): Unit =
    var last = new ListNode(-4)
    val head = new ListNode(3, new ListNode(2, new ListNode(0, last)))
    last.next = head.next
    println(detectCycle(head))
    
  private def detectCycle(head: ListNode): ListNode =
    if head == null || head.next == null then return null

    var (slow, fast) = (head, head)
    while fast.next != null && fast.next.next != null do
      slow = slow.next
      fast = fast.next.next
      if slow == fast then
        slow = head
        while slow != fast do
          slow = slow.next
          fast = fast.next
        return slow

    null
