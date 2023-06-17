package com.leetcode.cosminci._2800

import com.leetcode.cosminci.utils.UnionFind
import scala.util.chaining.*

object _2709_GcdTraversal:

  def canTraverseAllPairs(nums: Array[Int]): Boolean =
    val uf = new UnionFind[Int]

    nums.zipWithIndex.foldLeft(Map.empty[Int, Int]) { case (fst, (n, i)) =>
      Iterator
        .iterate((2, n, fst)) { case (p, x, fst) =>
          if x % p != 0 then (p + 1, x, fst)
          else Iterator.iterate(x)(_ / p).dropWhile(_ % p == 0).next().pipe { x =>
            fst.get(p) match
              case Some(j) => uf.union(j, i).pipe(_ => (p + 1, x, fst))
              case None    => (p + 1, x, fst.updated(p, i))
          }
        }
        .dropWhile { case (prime, x, fst) => prime * prime <= x }.next()
        .pipe { case (_, x, fst) =>
          if x == 1 then fst
          else fst.get(x) match
            case Some(j) => uf.union(j, i).pipe(_ => fst)
            case None    => fst.updated(x, i)
        }
    }

    uf.rank(uf.find(0)) == nums.length
