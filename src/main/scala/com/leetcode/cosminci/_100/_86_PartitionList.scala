package com.leetcode.cosminci._100

import com.leetcode.cosminci.utils.*

object _86_PartitionList:

  def partition(head: ListNode, x: Int): ListNode =
    val smallerHead = new ListNode(-1, null)
    val largerHead  = new ListNode(-1, null)

    val (_, smaller, larger) = Iterator
      .iterate((head, smallerHead, largerHead)) { case (curr, smaller, larger) =>
        if curr.x < x then
          smaller.next = curr
          (curr.next, smaller.next, larger)
        else
          larger.next = curr
          (curr.next, smaller, larger.next)
      }
      .dropWhile(_._1 != null)
      .next()

    larger.next = null
    smaller.next = largerHead.next
    smallerHead.next
