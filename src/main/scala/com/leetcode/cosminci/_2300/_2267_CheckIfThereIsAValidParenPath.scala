package com.leetcode.cosminci._2300

import scala.collection.mutable

object _2267_CheckIfThereIsAValidParenPath:

  def hasValidPath(grid: Array[Array[Char]]): Boolean =
    val mem = mutable.Map.empty[(Int, Int, Int), Boolean]
    def dfs(x: Int, y: Int, req: Int): Boolean = mem.getOrElseUpdate((x, y, req), {
      val balance = if grid(x)(y) == '(' then 1 else -1
      if req < 0 then false
      else if x + y + 1 < req.abs then false
      else if x == 0 && y == 0 then req == balance
      else if x == 0 then dfs(x, y - 1, req - balance)
      else if y == 0 then dfs(x - 1, y, req - balance)
      else dfs(x - 1, y, req - balance) || dfs(x, y - 1, req - balance)
    })

    dfs(x = grid.length - 1, y = grid.head.length - 1, req = 0)
