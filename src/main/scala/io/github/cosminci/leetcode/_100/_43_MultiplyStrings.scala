package io.github.cosminci.leetcode._100

object _43_MultiplyStrings:

  def main(args: Array[String]): Unit =
    println(multiply("123", "456"))

  def multiply(num1: String, num2: String): String =
    if num1 == "0" || num2 == "0" then return "0"
    val n1 = num1.map(_ - '0').reverse
    val n2 = num2.map(_ - '0').reverse

    var result = "0" * (num1.length + num2.length)
    var carry  = 0
    var index  = 0
    n1.zipWithIndex.foreach { case (d1, idx1) =>
      n2.zipWithIndex.foreach { case (d2, idx2) =>
        index = idx1 + idx2
        val prev    = (result(index) - '0')
        val product = d1 * d2 + carry + prev
        result = updateResult(result, index, product % 10)
        carry = product / 10
      }
      if carry > 0 then
        result = updateResult(result, index + 1, carry)
        carry = 0
    }

    result.reverse.dropWhile(_ == '0')

  def updateResult(result: String, index: Int, value: Int) =
    result.updated(index, ('0' + value).toChar)
