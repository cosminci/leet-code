package io.github.cosminci.leetcode._500

import io.github.cosminci.utils.ListNode

import scala.collection.mutable

object _445_AddTwoNumbersII:
  private def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode =
    def digits(head: ListNode): Seq[Int] =
      if head != null then digits(head.next) :+ head.x else Seq.empty

    val (resultCarry, resultHead) =
      digits(l1).zipAll(digits(l2), 0, 0).foldLeft[(Int, ListNode)]((0, null)) {
        case ((carry, nextNode), (l1digit, l2digit)) =>
          val sum = l1digit + l2digit + carry
          (if sum >= 10 then 1 else 0, new ListNode(sum % 10, nextNode))
      }

    if resultCarry == 0 then resultHead else new ListNode(1, resultHead)
