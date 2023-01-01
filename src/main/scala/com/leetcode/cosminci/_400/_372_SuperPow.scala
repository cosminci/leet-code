package com.leetcode.cosminci._400

object _372_SuperPow:

  def superPow(a: Int, b: Array[Int]): Int =
    b.lastOption match {
      case None       => 1
      case Some(last) => powMod(superPow(a, b.dropRight(1)), pow = 10) * powMod(a, last) % 1337
    }

  private def powMod(n: Int, pow: Int) =
    Iterator.iterate(1)(res => (res * (n % 1337)) % 1337).drop(pow).next()
