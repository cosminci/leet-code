package io.github.cosminci.leetcode._2400

import io.github.cosminci.utils.ListNode

object _2326_SpiralMatrixIV:

  def spiralMatrix(m: Int, n: Int, head: ListNode): Array[Array[Int]] =
    val arr = Array.fill(m, n)(-1)
    val dir = Array(0, 1, 0, -1, 0)

    @annotation.tailrec
    def dfs(x: Int, y: Int, d: Int, curr: ListNode): Unit =
      if curr == null then return
      arr(x)(y) = curr.x
      val (nx, ny) = (x + dir(d), y + dir(d + 1))
      if nx >= 0 && ny >= 0 && nx < m && ny < n && arr(nx)(ny) == -1 then dfs(nx, ny, d, curr.next)
      else
        val nd = (d + 1) % 4
        dfs(x + dir(nd), y + dir(nd + 1), nd, curr.next)

    dfs(x = 0, y = 0, d = 0, curr = head)
    arr
