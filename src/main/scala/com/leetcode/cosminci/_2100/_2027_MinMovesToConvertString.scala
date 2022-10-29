package com.leetcode.cosminci._2100

object _2027_MinMovesToConvertString:
  def main(args: Array[String]): Unit =
    println(minimumMoves("0000"))

  def minimumMoves(s: String): Int =
    s.indices
      .foldLeft(0, 0) { case ((moves, ignoreUntil), idx) =>
        if s(idx) == '0' || idx < ignoreUntil then
          (moves, math.max(ignoreUntil, idx + 1))
        else (moves + 1, idx + 3)
      }
      ._1
