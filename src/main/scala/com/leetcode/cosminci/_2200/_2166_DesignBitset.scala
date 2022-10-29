package com.leetcode.cosminci._2200

import scala.collection.mutable

object _2166_DesignBitset:

  class Bitset(size: Int):
    private var ones   = mutable.HashSet.empty[Int]
    private var zeroes = mutable.HashSet.from(0 until size)

    def fix(idx: Int): Unit =
      ones.add(idx)
      zeroes.remove(idx)

    def unfix(idx: Int): Unit =
      ones.remove(idx)
      zeroes.add(idx)

    def flip(): Unit =
      val tmp = ones
      ones = zeroes
      zeroes = tmp

    def all(): Boolean = ones.size == size

    def one(): Boolean = ones.nonEmpty

    def count(): Int = ones.size

    override def toString(): String =
      Array.tabulate(size)(idx => Option.when(ones.contains(idx))(1).getOrElse(0)).mkString
