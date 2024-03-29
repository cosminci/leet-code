package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils._

object _148_SortList:
  def main(args: Array[String]): Unit =
    println(linkedListToSeq(sortList(seqToLinkedList(Seq(3, -4, -3)))))

  def sortList(head: ListNode): ListNode =
    if head == null || head.next == null then return head

    val mid       = findMid(head)
    val rightHead = mid.next
    mid.next = null

    val left  = sortList(head)
    val right = sortList(rightHead)
    mergeTwoLists(left, right)

  def findMid(head: ListNode): ListNode =
    var (mid, curr) = (new ListNode(0, head), head)
    while curr != null && curr.next != null do
      curr = curr.next.next
      mid = mid.next
    mid

  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode =
    if l1 == null then return l2
    if l2 == null then return l1

    if l1.x <= l2.x then
      l1.next = mergeTwoLists(l1.next, l2); l1
    else
      l2.next = mergeTwoLists(l1, l2.next); l2
