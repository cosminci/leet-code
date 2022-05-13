package io.github.cosminci.leetcode._100

object _60_PermutationSequence:
  import _31_NextPermutation.nextPermutation

  def getPermutation(n: Int, k: Int): String =
    val permutation = (1 to n).toArray
    (0 until k).foreach(_ => nextPermutation(permutation))
    permutation.mkString
