package io.github.cosminci.leetcode._600

object _537_ComplexNumberMultiplication:
  def main(args: Array[String]): Unit =
    println(complexNumberMultiply("1+1i", "1+1i"))
    println(complexNumberMultiply("1+-1i", "1+-1i"))

  private def complexNumberMultiply(num1: String, num2: String): String =
    val (r1, i1) = tokenize(num1)
    val (r2, i2) = tokenize(num2)
    val r        = r1 * r2 - (i1 * i2)
    val i        = r1 * i2 + r2 * i1
    val rRaw     = if r == 0 then "0" else r
    val iRaw     = if i == 0 then "0" else i
    s"$rRaw+${iRaw}i"

  private def tokenize(n: String): (Int, Int) =
    val (rRaw, iRaw) = n.splitAt(n.indexOf('+'))
    (rRaw.toInt, iRaw.drop(1).dropRight(1).toInt)
