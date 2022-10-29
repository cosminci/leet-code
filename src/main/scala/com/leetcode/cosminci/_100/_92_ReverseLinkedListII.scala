package com.leetcode.cosminci._100

import com.leetcode.cosminci.utils.*

object _92_ReverseLinkedListII:

  def reverseBetween(head: ListNode, left: Int, right: Int): ListNode =
    val dummy    = new ListNode(0, head)
    val preStart = Iterator.iterate(dummy)(_.next).drop(left - 1).next()

    val (preLast, last) = Iterator
      .iterate((preStart.next, preStart.next.next)) { case (prev, curr) =>
        val next = curr.next
        curr.next = prev
        (curr, next)
      }
      .drop(right - left)
      .next()

    preStart.next.next = last
    preStart.next = preLast
    dummy.next
