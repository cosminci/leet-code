package com.leetcode.cosminci._200

import scala.collection.mutable

object _166_FractionalToRecurringDecimal:
  def main(args: Array[String]): Unit =
    println(fractionToDecimal(-1, -2147483648))
    println(fractionToDecimal(-50, 8))
    println(fractionToDecimal(1, 6))
    println(fractionToDecimal(4, 333))

  def fractionToDecimal(num: Int, den: Int): String =
    if num == 0 then return "0"

    val result = new StringBuilder()
    if (num < 0) ^ (den < 0) then result.append('-')
    val (numerator, denominator) = (math.abs(num.toLong), math.abs(den.toLong))

    result.append(numerator / denominator)

    var remainder = numerator % denominator
    if remainder == 0 then return result.toString

    result.append('.')

    val prevRemainders = mutable.Map(remainder -> result.length())
    while remainder != 0 do
      val nextFraction = remainder * 10 / denominator
      result.append(nextFraction)
      remainder = remainder * 10 % denominator

      if prevRemainders.contains(remainder) then
        result.insert(prevRemainders(remainder), '(')
        result.append(')')
        return result.toString

      prevRemainders.update(remainder, result.length)

    result.toString
