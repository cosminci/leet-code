package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _225_ImplementStackUsingQueue:

  class MyStack:
    private val queue = mutable.Queue.empty[Int]

    def push(x: Int): Unit =
      queue.enqueue(x)
      (1 until queue.size).foreach(_ => queue.enqueue(queue.dequeue()))

    def pop(): Int = queue.dequeue()

    def top(): Int = queue.head

    def empty(): Boolean = queue.isEmpty
