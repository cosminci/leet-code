package io.github.cosminci.leetcode._1900

object _1899_MergeTripletsToFormTripletTarget:
  private def mergeTriplets(triplets: Array[Array[Int]], target: Array[Int]): Boolean =
    val Array(x, y, z) = target
    val candidates = triplets.filter { case Array(a, b, c) =>
      a <= x && b <= y && c <= z
    }
    candidates.exists(_(0) == x) && candidates.exists(_(1) == y) && candidates.exists(_(2) == z)

  private def mergeTriplets2(triplets: Array[Array[Int]], target: Array[Int]): Boolean =
    val Array(x, y, z) = target
    var foundX         = false
    var foundY         = false
    var foundZ         = false
    triplets.foreach { case Array(a, b, c) =>
      if a <= x && b <= y && c <= z then
        if a == x then foundX = true
        if b == y then foundY = true
        if c == z then foundZ = true
      if foundX && foundY && foundZ then return true
    }
    false
