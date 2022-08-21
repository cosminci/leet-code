package io.github.cosminci.leetcode._2400

object _2381_ShiftingLettersII:

  def shiftingLetters(s: String, shifts: Array[Array[Int]]): String =
    val ramps = shifts
      .flatMap { case Array(start, end, direction) => Seq((start, direction), (end + 1, 1 - direction)) }
      .groupMapReduce(_._1)(s => if s._2 == 1 then 1 else -1)(_ + _)

    val prefixShift = s.indices
      .map(i => ramps.getOrElse(i, 0))
      .scanLeft(0)(_ + _)
      .tail

    s.zip(prefixShift).map { case (char, shift) => ('a' + (char - 'a' + 26 + shift % 26) % 26).toChar }.mkString
