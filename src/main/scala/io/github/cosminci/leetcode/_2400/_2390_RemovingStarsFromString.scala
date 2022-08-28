package io.github.cosminci.leetcode._2400

import scala.collection.mutable

object _2390_RemovingStarsFromString:

  def removeStars(s: String): String =
    val res = mutable.Stack.empty[Char]
    s.foreach(char => if char == '*' then res.pop() else res.push(char))
    res.mkString
