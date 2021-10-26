package io.github.cosminci.leetcode._100

import io.github.cosminci.utils.ListNode

object _21_MergeTwoSortedLists:
  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode =
    if l1 == null then return l2
    if l2 == null then return l1

    if l1.x <= l2.x then
      l1.next = mergeTwoLists(l1.next, l2); l1
    else
      l2.next = mergeTwoLists(l1, l2.next); l2
