package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.*

object _143_ReorderList:

  def main(args: Array[String]): Unit =
    val head = seqToLinkedList(Seq(1, 2, 3, 4, 5))
    reorderList(head)
    println(linkedListToSeq(head))

  def reorderList(head: ListNode): Unit =
    @annotation.tailrec
    def findMiddle(slow: ListNode, fast: ListNode): ListNode =
      if fast.next == null || fast.next.next == null then slow
      else findMiddle(slow.next, fast.next.next)

    @annotation.tailrec
    def reverse(prev: ListNode, curr: ListNode): ListNode =
      if curr == null then prev
      else
        val next = curr.next
        curr.next = prev
        reverse(curr, next)

    @annotation.tailrec
    def merge(head1: ListNode, head2: ListNode): Unit =
      if head2 == null then ()
      else
        val next = head1.next
        head1.next = head2
        merge(head2, next)

    val mid  = findMiddle(slow = head, fast = head)
    val last = reverse(prev = null, curr = mid.next)
    mid.next = null
    merge(head1 = head, head2 = last)
