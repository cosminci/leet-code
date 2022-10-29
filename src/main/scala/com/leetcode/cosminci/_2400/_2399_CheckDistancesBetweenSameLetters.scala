package com.leetcode.cosminci._2400

object _2399_CheckDistancesBetweenSameLetters {

  def checkDistances(s: String, distance: Array[Int]): Boolean =
    ('a' to 'z').forall { c =>
      val (i, j) = (s.indexOf(c), s.lastIndexOf(c))
      (i == -1) || distance(c - 'a') == j - i - 1
    }
}
