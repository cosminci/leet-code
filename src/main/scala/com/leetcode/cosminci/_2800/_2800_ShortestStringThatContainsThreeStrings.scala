package com.leetcode.cosminci._2800

object _2800_ShortestStringThatContainsThreeStrings:

  def minimumString(a: String, b: String, c: String): String =
    List((a, b, c), (b, c, a), (c, a, b))
      .map { case (a, b, c) => solve(a, b, c) }
      .reduce { case (x, y) => min(x, y) }

  private def solve(a: String, b: String, c: String) =
    val (t1, t2) = (add(a, b), add(b, a))
    min(min(add(t1, c), add(c, t1)), min(add(t2, c), add(c, t2)))

  private def add(a: String, b: String) =
    if b.contains(a) then b
    else (0 until a.length)
      .collectFirst { case i if b.startsWith(a.substring(i)) => s"$a${b.substring(a.length - i)}" }
      .getOrElse(s"$a$b")

  private def min(a: String, b: String) =
    if a.length < b.length then a
    else if a.length == b.length && a < b then a
    else b
