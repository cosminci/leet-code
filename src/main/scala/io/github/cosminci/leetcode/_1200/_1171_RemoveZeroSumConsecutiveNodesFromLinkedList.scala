package io.github.cosminci.leetcode._1200

import io.github.cosminci.utils.ListNode

import scala.collection.mutable

object _1171_RemoveZeroSumConsecutiveNodesFromLinkedList:
  def main(args: Array[String]): Unit =
    val head   = new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(-3, new ListNode(-2)))))
    val result = removeZeroSumSublists(head)
    println()

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
