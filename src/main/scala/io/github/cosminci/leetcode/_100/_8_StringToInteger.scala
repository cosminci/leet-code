package io.github.cosminci.leetcode._100

object _8_StringToInteger:

  def main(args: Array[String]): Unit =
    println(myAtoi("20000000000000000000"))

  private def myAtoi(s: String): Int =
    val trimmed = s.trim
    if trimmed == "" then return 0
    val (sign, trimmed2) =
      if trimmed.head == '-' then ("-", trimmed.tail)
      else if trimmed.head == '+' then ("", trimmed.tail)
      else ("", trimmed)

    val digits = trimmed2.takeWhile(c => c >= '0' && c <= '9')
    if digits == "" then return 0

    val res = BigInt(s"$sign$digits")
    if res > Int.MaxValue then Int.MaxValue
    else if res < Int.MinValue then Int.MinValue
    else res.toInt
