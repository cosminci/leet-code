package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.ListNode

object _142_LinkedListCycleII:
  def main(args: Array[String]): Unit =
    val last = new ListNode(-4)
    val head = new ListNode(3, new ListNode(2, new ListNode(0, last)))
    last.next = head.next
    println(detectCycle(head))

  def detectCycle(head: ListNode): ListNode =
    @annotation.tailrec
    def findCycle(slow: ListNode, fast: ListNode): ListNode =
      if fast.next == null || fast.next.next == null then null
      else
        val (nextSlow, nextFast) = (slow.next, fast.next.next)
        if nextSlow == nextFast then findCycleStart(head, nextFast)
        else findCycle(nextSlow, nextFast)

    @annotation.tailrec
    def findCycleStart(slow: ListNode, fast: ListNode): ListNode =
      if slow == fast then slow
      else findCycleStart(slow.next, fast.next)

    Option.when(head != null && head.next != null)(findCycle(head, head)).orNull
