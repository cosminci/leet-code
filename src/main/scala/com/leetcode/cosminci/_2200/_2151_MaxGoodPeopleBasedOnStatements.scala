package com.leetcode.cosminci._2200

object _2151_MaxGoodPeopleBasedOnStatements:

  def maximumGood(statements: Array[Array[Int]]): Int = {
    val n = statements.length

    def statementsHoldForPerson(bitset: Int, i: Int) =
      (0 until n).forall(j => statements(i)(j) == 2 || statements(i)(j) == (bitset >> j & 1))

    def statementsHoldForAll(bitset: Int): Boolean =
      (0 until n)
        .filter(i => (bitset >> i & 1) == 1)
        .forall(i => statementsHoldForPerson(bitset, i))

    @annotation.tailrec
    def countOneBits(bitset: Int, bits: Int): Int =
      if (bitset == 0) bits
      else countOneBits(bitset >> 1, bits + (bitset & 1))

    (0 until 1 << n).collect {
      case bitset if statementsHoldForAll(bitset) =>
        countOneBits(bitset, 0)
    }.max
  }
