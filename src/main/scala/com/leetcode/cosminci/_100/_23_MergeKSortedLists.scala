package com.leetcode.cosminci._100

import com.leetcode.cosminci.utils.*

import scala.collection.mutable

object _23_MergeKSortedLists:

  def main(args: Array[String]): Unit =
    println(linkedListToSeq(mergeKLists(Array(seqToLinkedList(Seq(1, 4)), seqToLinkedList(Seq(3)), seqToLinkedList(Seq(2, 3, 5))))))

  def mergeKLists(lists: Array[ListNode]): ListNode =
    val nonNullLists = lists.flatMap(head => Option(head))
    val pqueue       = mutable.PriorityQueue.from(nonNullLists)(Ordering.by(node => -node.x))
    val dummyHead    = new ListNode()

    @annotation.tailrec
    def dfs(prev: ListNode): ListNode =
      if pqueue.isEmpty then dummyHead.next
      else
        val curr = pqueue.dequeue()
        prev.next = curr
        Option(curr.next).foreach(tail => pqueue.enqueue(tail))
        dfs(curr)

    dfs(dummyHead)
