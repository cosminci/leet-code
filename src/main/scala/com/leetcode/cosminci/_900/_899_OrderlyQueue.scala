package com.leetcode.cosminci._900

import scala.util.chaining._

object _899_OrderlyQueue:

  def orderlyQueue(s: String, k: Int): String =
    def rotate(i: Int): String =
      s.splitAt(i).pipe { case (fh, sh) => s"$sh$fh" }

    if k > 1 then s.sorted else s.indices.map(rotate).min
