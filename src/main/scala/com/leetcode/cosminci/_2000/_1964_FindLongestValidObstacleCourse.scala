package com.leetcode.cosminci._2000

object _1964_FindLongestValidObstacleCourse:

  def longestObstacleCourseAtEachPosition(obstacles: Array[Int]): Array[Int] =
    val stack = Array.fill(obstacles.length)(0)
    val res   = Array.fill(obstacles.length)(0)

    obstacles.indices.foldLeft(0) { case (len, i) =>
      val (l, r) = Iterator
        .iterate((0, len)) { case (l, r) =>
          val mid = l + (r - l) / 2
          if stack(mid) <= obstacles(i) then (mid + 1, r) else (l, mid)
        }
        .dropWhile { case (l, r) => l < r }.next()
      res(i) = l + 1
      stack(l) = obstacles(i)
      if len == l then len + 1 else len
    }

    res
