package io.github.cosminci.leetcode._2500

import scala.util.chaining.*

object _2429_MinimizeXor:

  def minimizeXor(num1: Int, num2: Int): Int =
    @annotation.tailrec
    def bitCount(n: Int, cnt: Int): Int =
      if n == 0 then cnt else bitCount(n / 2, cnt + Option.when(n % 2 == 0)(0).getOrElse(1))

    @annotation.tailrec
    def dropBits(n: Int, bit: Int, budget: Int): Int =
      if budget == 0 then n
      else if (num1 >> bit & 1) == 0 then dropBits(n, bit + 1, budget)
      else dropBits(n ^ (1 << bit), bit + 1, budget - 1)

    @annotation.tailrec
    def setBits(n: Int, bit: Int, budget: Int): Int =
      if budget == 0 then n
      else if (num1 >> bit & 1) == 1 then setBits(n, bit + 1, budget)
      else setBits(n | (1 << bit), bit + 1, budget - 1)

    val (cnt1, cnt2) = (bitCount(num1, cnt = 0), bitCount(num2, cnt = 0))

    num1
      .pipe(dropBits(_, bit = 0, (cnt1 - cnt2).max(0)))
      .pipe(setBits(_, bit = 0, (cnt2 - cnt1).max(0)))
