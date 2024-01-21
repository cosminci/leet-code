package com.leetcode.cosminci._3100

object _3001_MinMovesToCaptureQueen:

  def minMovesToCaptureTheQueen(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int): Int =
    @annotation.tailrec
    def dfs(toVisit: Seq[(Int, Int, Int, Int)], visited: Set[(Int, Int, Int, Int)], steps: Int): Int =
      if toVisit.exists { case (a, b, c, d) => (a == e && b == f) || (c == e && d == f) } then steps
      else
        val next = toVisit.flatMap(nextPos).filterNot(visited.contains)
        dfs(toVisit ++ next, visited ++ next, steps + 1)

    dfs(toVisit = Seq((a, b, c, d)), visited = Set((a, b, c, d)), steps = 0)

  private def nextPos(pos: (Int, Int, Int, Int)): Seq[(Int, Int, Int, Int)] =
    val (a, b, c, d) = pos
    val rookPos      = Seq((1, 0), (0, 1), (-1, 0), (0, -1)).flatMap { case (dx, dy) => move(a, b, dx, dy, c, d) }
    val bishopPos    = Seq((1, 1), (-1, -1), (-1, 1), (1, -1)).flatMap { case (dx, dy) => move(c, d, dx, dy, a, b) }
    rookPos.map { case (a, b) => (a, b, c, d) } ++ bishopPos.map { case (c, d) => (a, b, c, d) }

  private def move(x: Int, y: Int, dx: Int, dy: Int, u: Int, v: Int) =
    Iterator
      .iterate((x, y)) { case (x, y) => (x + dx, y + dy) }
      .takeWhile { case p @ (x, y) => p != (u, v) && x.min(y) >= 1 && x.max(y) <= 8 }
      .toSeq
      .tail
