package io.github.cosminci.leetcode._100

import scala.util.chaining.*

object _76_MinWindowSubstring:

  def main(args: Array[String]): Unit = {
    println(minWindow("", ""))
  }

  def minWindow(s: String, t: String): String =
    val need = t.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)

    def dfs(l: Int, r: Int, window: Map[Char, Int], bestL: Int, bestR: Int): (Int, Int) =
      Option.when(r - l < bestR - bestL)((l, r)).getOrElse((bestL, bestR)).pipe { case (bestL, bestR) =>
        if window(s(l)) > need(s(l)) then dfs(l + 1, r, window.updated(s(l), window(s(l)) - 1), bestL, bestR)
        else if r == s.length then (bestL, bestR)
        else dfs(l, r + 1, window.updated(s(r), window(s(r)) + 1), bestL, bestR)
      }

    @annotation.tailrec
    def getInitialRight(i: Int, window: Map[Char, Int]): (Int, Map[Char, Int]) =
      if need.forall { case (ch, cnt) => window(ch) >= cnt } then (i, window)
      else if i == s.length then (-1, window)
      else getInitialRight(i + 1, window.updated(s(i), window(s(i)) + 1))

    getInitialRight(i = 0, window = Map.empty.withDefaultValue(0)).pipe {
      case (r0, _) if r0 <= 0 => ""
      case (r0, window) => dfs(l = 0, r = r0, window, bestL = 0, bestR = r0).pipe(s.slice)
    }
