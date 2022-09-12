package io.github.cosminci.leetcode._2500

object _2407_LongestIncreasingSubsequenceII:

  // https://codeforces.com/blog/entry/18051
  class SegmentTree(n: Int):
    private val tree = Array.fill(n * 2)(0)

    def query(l: Int, r: Int): Int =
      Iterator
        .iterate((l + n, r + n, 0)) { case (l, r, res) =>
          val (nl, lres) = if (l & 1) > 0 then (l + 1, tree(l)) else (l, 0)
          val (nr, rres) = if (r & 1) > 0 then (r - 1, tree(r - 1)) else (r, 0)
          (nl >> 1, nr >> 1, res.max(lres).max(rres))
        }
        .dropWhile { case (l, r, _) => l < r }
        .next()._3

    def update(i: Int, value: Int): Unit =
      Iterator
        .iterate({ tree(i + n) = value; i + n }) { i =>
          tree(i >> 1) = tree(i).max(tree(i ^ 1))
          i >> 1
        }
        .dropWhile(_ > 1)
        .next()

  def lengthOfLIS(nums: Array[Int], k: Int): Int =
    val segmentTree = new SegmentTree(nums.max)
    nums.foldLeft(1) { (res, n) =>
      val prevMax = segmentTree.query((n - 1 - k).max(0), n - 1)
      segmentTree.update(n - 1, prevMax + 1)
      res.max(prevMax + 1)
    }
