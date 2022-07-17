package io.github.cosminci.leetcode._2100

import io.github.cosminci.utils.*

object _2095_DeleteTheMiddleNodeOfALinkedList:
  def main(args: Array[String]): Unit =
    println(linkedListToSeq(deleteMiddle(seqToLinkedList(Seq(1, 3, 4, 7, 1, 2, 6)))))
    println(linkedListToSeq(deleteMiddle(seqToLinkedList(Seq(1, 2, 3, 4)))))
    println(linkedListToSeq(deleteMiddle(seqToLinkedList(Seq(2, 1)))))
    println(linkedListToSeq(deleteMiddle(seqToLinkedList(Seq(2)))))

  def deleteMiddle(head: ListNode): ListNode =
    @annotation.tailrec
    def dfs(slow: ListNode, fast: ListNode): ListNode =
      if fast == null || fast.next == null then slow
      else dfs(slow.next, fast.next.next)

    val dummyHead = new ListNode(0, head)
    val prevMid   = dfs(dummyHead, head)
    prevMid.next = prevMid.next.next
    dummyHead.next
