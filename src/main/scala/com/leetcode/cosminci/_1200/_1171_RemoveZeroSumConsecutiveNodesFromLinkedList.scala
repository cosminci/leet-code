package com.leetcode.cosminci._1200

import com.leetcode.cosminci.utils._

import scala.collection.mutable

object _1171_RemoveZeroSumConsecutiveNodesFromLinkedList:
  def main(args: Array[String]): Unit =
    println(linkedListToSeq(removeZeroSumSublists(seqToLinkedList(Seq(1, 2, 3, -3, -2)))))

  def removeZeroSumSublists(head: ListNode): ListNode =
    val dummy      = new ListNode(0, head)
    val prefixSums = mutable.Map(0 -> dummy)

    var rollingSum = 0
    var curr       = dummy

    while curr.next != null do
      curr = curr.next
      rollingSum += curr.x
      if !prefixSums.contains(rollingSum) then prefixSums.update(rollingSum, curr)
      else
        val slicePreHead    = prefixSums(rollingSum)
        var sliceRollingSum = rollingSum
        var sliceCurr       = slicePreHead.next
        while sliceCurr != curr do
          sliceRollingSum += sliceCurr.x
          prefixSums.remove(sliceRollingSum)
          sliceCurr = sliceCurr.next
        slicePreHead.next = curr.next

    dummy.next
