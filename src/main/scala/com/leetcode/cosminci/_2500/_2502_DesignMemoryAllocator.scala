package com.leetcode.cosminci._2500

import scala.util.chaining.*

object _2502_DesignMemoryAllocator:

  class Allocator(n: Int):

    private val memory = Array.fill(n)(-1)

    def allocate(size: Int, mID: Int): Int =
      memory.zipWithIndex.foldLeft(0) { case (freeStreak, (allocatedTo, i)) =>
        if allocatedTo != -1 then 0
        else (freeStreak + 1).pipe { freeStreak =>
          if freeStreak != size then freeStreak
          else
            (i - size + 1 to i).foreach(memory(_) = mID)
            return i - size + 1
        }
      }.pipe(_ => -1)

    def free(mID: Int): Int =
      memory.zipWithIndex.foldLeft(0) { case (freed, (allocatedTo, i)) =>
        if allocatedTo != mID then freed
        else
          memory(i) = -1
          freed + 1
      }
