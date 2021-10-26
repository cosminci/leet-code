package io.github.cosminci.leetcode._1000

import scala.collection.mutable

object _953_VerifyingAnAlienDictionary:

  def main(args: Array[String]): Unit =
    println(isAlienSorted(Array("hello", "leetcode"), "hlabcdefgijkmnopqrstuvwxyz"))

  private def isAlienSorted(words: Array[String], order: String): Boolean =
    val charIndices = mutable.Map.empty[Char, Int]
    order.indices.foreach(i => charIndices.update(order(i), i))
    (0 until words.length - 1).foreach { i =>
      val (w1, w2) = (words(i), words(i + 1))
      if !inOrder(w1, w2, charIndices) then return false
    }
    true

  private def inOrder(w1: String, w2: String, charIndices: mutable.Map[Char, Int]): Boolean =
    var idx = 0
    while idx < w1.length && idx < w2.length && charIndices(w1(idx)) == charIndices(w2(idx)) do idx += 1
    idx == w1.length || (idx < w2.length && charIndices(w1(idx)) < charIndices(w2(idx)))
