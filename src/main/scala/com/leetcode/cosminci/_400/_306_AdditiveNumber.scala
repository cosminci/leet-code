package com.leetcode.cosminci._400

object _306_AdditiveNumber:
  def main(args: Array[String]): Unit =
    Seq("112358", "199100199", "101", "000", "0235813", "1991000199299498797")
      .foreach(num => println(isAdditiveNumber(num)))

  def isAdditiveNumber(num: String): Boolean =
    def checkBootstrapped(indices: Seq[Int]): Boolean =
      var Seq(i, j) = indices

      var (a, b) = (num.substring(0, i), num.substring(i, j))
      if a != BigInt(a).toString || b != BigInt(b).toString then return false

      while j < num.length do
        var c = (BigInt(a) + BigInt(b)).toString
        if !num.startsWith(c, j) then return false
        j += c.length
        a = b
        b = c

      true

    (1 until num.length / 3 * 2 + 1).combinations(2).exists(checkBootstrapped)
