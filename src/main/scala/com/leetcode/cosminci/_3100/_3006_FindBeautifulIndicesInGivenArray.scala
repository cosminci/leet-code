package com.leetcode.cosminci._3100

object _3006_FindBeautifulIndicesInGivenArray:

  def beautifulIndices(s: String, a: String, b: String, k: Int): List[Int] =
    val aIndices = (0 to s.length - a.length).filter(i => s.startsWith(a, i))
    val bIndices = (0 to s.length - b.length).filter(i => s.startsWith(b, i))

    @annotation.tailrec
    def search(i: Int, l: Int, r: Int): Boolean =
      if l >= r then false
      else
        val mid = l + (r - l) / 2
        val j   = bIndices(mid)
        if (i - j).abs <= k then true
        else if i < j then search(i, l, mid)
        else search(i, mid + 1, r)

    aIndices.filter(i => search(i, l = 0, r = bIndices.length)).toList
