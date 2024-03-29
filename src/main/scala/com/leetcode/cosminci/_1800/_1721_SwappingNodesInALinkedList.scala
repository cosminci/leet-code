package com.leetcode.cosminci._1800

import com.leetcode.cosminci.utils.{ListNode, seqToLinkedList}

object _1721_SwappingNodesInALinkedList:

  def swapNodes(head: ListNode, k: Int): ListNode =
    val kthFromHead = Iterator.iterate(head)(_.next).drop(k - 1).next()
    val kthFromEnd = Iterator
      .iterate((head, kthFromHead.next)) { case (kBehind, endCanary) => (kBehind.next, endCanary.next) }
      .dropWhile { case (_, curr) => curr != null }
      .next()._1

    val tmp = kthFromHead.x
    kthFromHead.x = kthFromEnd.x
    kthFromEnd.x = tmp
    head
