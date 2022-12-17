package com.leetcode.cosminci._300

import scala.collection.mutable

object _232_ImplementQueueUsingStacks {

  class MyQueue {

    private val stack1 = mutable.Stack[Int]()
    private val stack2 = mutable.Stack[Int]()

    def push(x: Int): Unit = {
      if (!empty()) moveBack()
      stack1.push(x)
    }

    def pop(): Int = if (hasNext()) stack2.pop else -1

    def peek(): Int = if (hasNext()) stack2.top else -1

    def empty(): Boolean = stack1.isEmpty && stack2.isEmpty

    private def hasNext(): Boolean = {
      if (stack1.nonEmpty) move()
      !empty()
    }

    private def move(): Unit =
      while (stack1.nonEmpty) stack2.push(stack1.pop)

    private def moveBack(): Unit =
      while (stack2.nonEmpty) stack1.push(stack2.pop)

  }
}
