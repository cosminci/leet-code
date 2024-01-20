package com.leetcode.cosminci._3000

object _2981_FindLongestSpecialSubstrThatOccursThriceI:

  def maximumLength(s: String): Int =
    group(s)
      .flatMap(s => Seq(s -> 1) ++ Option.when(s.length > 1)(s.tail -> 2) ++ Option.when(s.length > 2)(s.drop(2) -> 3))
      .groupMapReduce { case (s, _) => s } { case (_, c) => c }(_ + _)
      .collect { case (s, cnt) if cnt >= 3 => s.length }
      .maxOption.getOrElse(-1)

  @annotation.tailrec
  private def group(s: String, res: Seq[String] = Seq.empty): Seq[String] =
    if s.isEmpty then res
    else
      val (same, rest) = s.span(_ == s.head)
      group(rest, res :+ same)
