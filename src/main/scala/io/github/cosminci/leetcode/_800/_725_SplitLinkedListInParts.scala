package io.github.cosminci.leetcode._800

import io.github.cosminci.utils._

object _725_SplitLinkedListInParts:
  def main(args: Array[String]): Unit =
    splitListToParts(linkedList(Seq(1, 2, 3)), 2).map(seq).foreach(println)

  def splitListToParts(head: ListNode, k: Int): Array[ListNode] =
    def length(node: ListNode): Int = if node == null then 0 else 1 + length(node.next)
    def copyList(node: ListNode, size: Int): (ListNode, ListNode) =
      if size == 0 then (null, null)
      else if size == 1 then (new ListNode(node.x), node.next)
      else
        val (next, nextHead) = copyList(node.next, size - 1)
        (new ListNode(node.x, next), nextHead)

    val n = length(head)

    val partSizes = Seq.fill(n % k)(n / k + 1) ++ Seq.fill(k - n % k)(n / k)
    partSizes
      .foldLeft(Seq.empty[ListNode], head) { case ((lists, nextHead), partSize) =>
        val (newList, newNextHead) = copyList(nextHead, partSize)
        (lists :+ newList, newNextHead)
      }._1.toArray
