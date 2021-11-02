package io.github.cosminci.leetcode._100

import io.github.cosminci.utils._

object _23_MergeKSortedLists:

  def main(args: Array[String]): Unit =
    println(seq(mergeKLists(Array(linkedList(Seq(1, 4)), linkedList(Seq(3)), linkedList(Seq(2, 3, 5))))))

  private def mergeKLists(lists: Array[ListNode]): ListNode = lists match
    case Array()             => null
    case Array(singleList)   => singleList
    case Array(list1, list2) => merge2Lists(list1, list2)
    case _ =>
      val (fh, sh) = lists.splitAt(lists.length / 2)
      merge2Lists(mergeKLists(fh), mergeKLists(sh))

  private def merge2Lists(list1: ListNode, list2: ListNode): ListNode =
    if list1 == null then return list2
    if list2 == null then return list1

    var (l1, l2) = (list1, list2)
    val head = if l1.x <= l2.x then
      val tmp = l1; l1 = l1.next; tmp
    else
      val tmp = l2; l2 = l2.next; tmp
    var prev = head
    while l1 != null && l2 != null do
      if l1.x <= l2.x then
        prev.next = l1
        prev = prev.next
        l1 = l1.next
      else
        prev.next = l2
        prev = prev.next
        l2 = l2.next
    if l1 == null then prev.next = l2
    else prev.next = l1

    head

  private def mergeKListsOneElementAtATime(lists: Array[ListNode]): ListNode =
    if lists.isEmpty then return null

    var head: ListNode = null
    var prev: ListNode = null

    val zippedLists = lists.zipWithIndex
    while true do
      val min = competeMin(zippedLists)
      if min._1 == null then return head
      if head == null then
        head = new ListNode(min._1.x)
        prev = head
      else
        prev.next = new ListNode(min._1.x)
        prev = prev.next
      zippedLists(min._2) = (min._1.next, min._2)

    head

  private def competeMin(values: Array[(ListNode, Int)]): (ListNode, Int) =
    values match
      case Array(n)             => n
      case Array((null, _), n2) => n2
      case Array(n1, (null, _)) => n1
      case Array(n1, n2) =>
        if n1._1.x <= n2._1.x then n1 else n2
      case _ =>
        val (firstHalf, secondHalf) = values.splitAt(values.length / 2)
        val fh @ (minFirstHalf, _)  = competeMin(firstHalf)
        val sh @ (minSecondHalf, _) = competeMin(secondHalf)
        if minFirstHalf == null then sh
        else if minSecondHalf == null then fh
        else if minFirstHalf.x <= minSecondHalf.x then fh
        else sh
