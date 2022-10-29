package com.leetcode.cosminci._500

object _415_AddStrings:
  def main(args: Array[String]): Unit =
    println(addStrings("9", "91"))

  def addStrings(num1: String, num2: String): String =
    val (n1, n2) =
      if num1.length < num2.length then (num1.reverse.padTo(num2.length, '0').reverse, num2)
      else (num1, num2.reverse.padTo(num1.length, '0').reverse)

    var carry  = 0
    val result = new StringBuilder
    (n1.length - 1 to 0 by -1).foreach { i =>
      val sum = (n1(i) - '0') + (n2(i) - '0') + carry
      carry = sum / 10
      result.append(('0' + (sum % 10)).toChar)
    }
    if carry == 1 then result.append('1')

    result.reverse.toString
