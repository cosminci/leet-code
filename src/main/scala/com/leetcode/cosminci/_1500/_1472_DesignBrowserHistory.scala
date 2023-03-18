package com.leetcode.cosminci._1500

import scala.collection.mutable

object _1472_DesignBrowserHistory:

  final class BrowserHistory(homepage: String):
    private val prev = mutable.Stack(homepage)
    private val next = mutable.Stack.empty[String]

    def visit(url: String): Unit =
      prev.push(url)
      next.clear()

    @annotation.tailrec
    def back(steps: Int): String =
      if steps == 0 || prev.size == 1 then prev.head
      else
        next.push(prev.pop())
        back(steps - 1)

    @annotation.tailrec
    def forward(steps: Int): String =
      if steps == 0 || next.isEmpty then prev.head
      else
        prev.push(next.pop())
        forward(steps - 1)
