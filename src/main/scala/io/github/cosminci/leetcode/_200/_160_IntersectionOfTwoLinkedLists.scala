package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.ListNode

object _160_IntersectionOfTwoLinkedLists:

  def getIntersectionNode(headA: ListNode, headB: ListNode): ListNode =
    Iterator
      .iterate((headA, headB)) { case (a, b) =>
        (if a == null then headB else a.next, if b == null then headA else b.next)
      }
      .dropWhile { case (a, b) => a != b }
      .next()._1
