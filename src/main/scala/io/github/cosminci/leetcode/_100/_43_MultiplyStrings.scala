package io.github.cosminci.leetcode._100

object _43_MultiplyStrings:

  def main(args: Array[String]): Unit =
    println(multiply("123", "456"))

  def multiply(num1: String, num2: String): String =
    val resultDigits = Array.fill(num1.length + num2.length)(0)

    for
      i <- num1.length - 1 to 0 by -1
      j <- num2.length - 1 to 0 by -1
    do
      val product = (num1(i) - '0') * (num2(j) - '0')
      val p1      = i + j
      val p2      = i + j + 1
      val sum     = product + resultDigits(p2)
      resultDigits(p1) += sum / 10
      resultDigits(p2) = sum % 10

    val result = resultDigits.mkString.dropWhile(_ == '0')
    if result.length == 0 then "0" else result
