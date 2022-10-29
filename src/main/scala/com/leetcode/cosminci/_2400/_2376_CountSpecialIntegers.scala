package com.leetcode.cosminci._2400

import scala.collection.mutable

object _2376_CountSpecialIntegers:

  def countSpecialNumbers(n: Int): Int =
    val digits = n.toString.map(_ - '0')
    val mem    = mutable.Map.empty[(Int, Int, Boolean), Int]

    def dfs(i: Int, mask: Int, tight: Boolean): Int = mem.getOrElseUpdate((i, mask, tight),
      if i == digits.length then Option.when(mask == 0)(0).getOrElse(1)
      else
        val limit = if tight then digits(i) else 9
        (0 to limit).collect {
          case d if ((mask >> d) & 1) == 0 =>
            val newMask = if mask == 0 && d == 0 then mask else mask | (1 << d)
            dfs(i + 1, newMask, d == limit && tight)
        }.sum
    )

    dfs(i = 0, mask = 0, tight = true)
