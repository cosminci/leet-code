package com.leetcode.cosminci._300

import com.leetcode.cosminci.utils.*

object _234_PalindromeLinkedList:

  def isPalindrome(head: ListNode): Boolean =
    def traverse(rev: ListNode, slow: ListNode, fast: ListNode) =
      val (nextRev, nextSlow, nextFast) = (slow, slow.next, fast.next.next)
      nextRev.next = rev
      (nextRev, nextSlow, nextFast)

    val (rev, slow, fast) = Iterator
      .iterate((null: ListNode, head, head))(traverse)
      .dropWhile { case (_, _, fast) => fast != null && fast.next != null }
      .next()

    Iterator
      .iterate((rev, Option(fast).map(_ => slow.next).getOrElse(slow))) { case (rev, slow) => (rev.next, slow.next) }
      .dropWhile { case (rev, slow) => rev != null && rev.x == slow.x }
      .next()
      ._1 == null
