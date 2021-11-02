package io.github.cosminci.leetcode._100

import io.github.cosminci.utils._

import scala.math.Integral.Implicits.*

object _2_AddTwoNumbers:

  def main(args: Array[String]): Unit =
    println(seq(addTwoNumbers(linkedList(Seq(9, 9, 9)), linkedList(Seq(9)))))

  private def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode =
    def dfs(h1: ListNode, h2: ListNode, carry: Int): ListNode =
      if h1 == null && h2 == null then
        return if carry == 0 then null else new ListNode(1)

      val v1 = if h1 == null then 0 else h1.x
      val v2 = if h2 == null then 0 else h2.x

      val sum       = (v1 + v2 + carry) % 10
      val nextCarry = if v1 + v2 + carry >= 10 then 1 else 0
      val h1Next    = if h1 == null then h1 else h1.next
      val h2Next    = if h2 == null then h2 else h2.next

      new ListNode(sum, dfs(h1Next, h2Next, nextCarry))

    dfs(l1, l2, carry = 0)
