package com.leetcode.cosminci._3000

object _2906_ConstructProductMatrix:

  def constructProductMatrix(grid: Array[Array[Int]]): Array[Array[Int]] =
    val mod    = 12345
    val (m, n) = (grid.length, grid.head.length)

    val prefixProduct = grid.flatten.scanLeft(1L)((acc, v) => (acc * v) % mod).dropRight(1)
    val suffixProduct = grid.flatten.scanRight(1L)((v, acc) => (acc * v) % mod).tail

    Array.tabulate(m) { r =>
      Array.tabulate(n) { c =>
        (prefixProduct(r * n + c) * suffixProduct(r * n + c) % mod).toInt
      }
    }
