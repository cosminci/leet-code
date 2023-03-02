package com.leetcode.cosminci._500

import scala.util.chaining.*

object _443_StringCompression:

  def compress(chars: Array[Char]): Int =
    if chars.length <= 1 then chars.length
    else
      chars.zipWithIndex
        .foldLeft(0, 0) { case ((anchor, write), (ch, i)) =>
          if i < chars.length - 1 && chars(i) == chars(i + 1) then (anchor, write)
          else (chars(write) = ch).pipe { _ =>
            if i == anchor then (i + 1, write + 1)
            else (i - anchor + 1).toString
              .foldLeft(write + 1)((write, cnt) => (chars(write) = cnt).pipe(_ => write + 1))
              .pipe(write => (i + 1, write))
          }
        }
        .pipe { case (_, write) => write }
