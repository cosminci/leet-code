package io.github.cosminci.leetcode._300

object _219_ContainsDuplicateII:

  def containsNearbyDuplicate(nums: Array[Int], k: Int): Boolean =
    nums.zipWithIndex.groupMap(_._1)(_._2).exists {
      case (num, indices) =>
        indices.length > 1 && indices.sliding(2).exists {
          case Array(i, j) => j - i <= k
        }
    }
