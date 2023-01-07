package com.leetcode.cosminci._2600

object _2526_FindConsecIntsFromDataStream:

  class DataStream(value: Int, k: Int):
    var cnt = 0
    def consec(num: Int): Boolean =
      if num == value then cnt += 1 else cnt = 0
      cnt >= k
