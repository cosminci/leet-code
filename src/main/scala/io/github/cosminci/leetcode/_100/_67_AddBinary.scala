package io.github.cosminci.leetcode._100

import scala.math.Integral.Implicits.*

object _67_AddBinary:
  def main(args: Array[String]): Unit =
    println(addBinary("110", "11"))

  def addBinary(a: String, b: String): String =
    val (result, carry) = a.reverse.zipAll(b.reverse, '0', '0').foldLeft("", 0) { case ((result, carry), (c1, c2)) =>
      val (quotient, remainder) = (c1 - '0' + c2 - '0' + carry) /% 2
      (('0' + remainder).toChar +: result, quotient)
    }
    if carry == 0 then result else '1' +: result
