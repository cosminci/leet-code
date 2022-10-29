package com.leetcode.cosminci._400

object _383_RandomNote:

  def canConstruct(ransomNote: String, magazine: String): Boolean =
    val available = magazine.groupMapReduce(identity)(_ => 1)(_ + _)
    ransomNote.groupMapReduce(identity)(_ => 1)(_ + _).forall { case (char, required) =>
      available.getOrElse(char, 0) >= required
    }
