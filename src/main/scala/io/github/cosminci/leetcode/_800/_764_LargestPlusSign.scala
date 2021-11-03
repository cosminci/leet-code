package io.github.cosminci.leetcode._800

object _764_LargestPlusSign:
  def main(args: Array[String]): Unit =
    println(orderOfLargestPlusSign(5, Array(Array(3, 0), Array(3, 3))))
    println(orderOfLargestPlusSign(5, Array(Array(4, 2))))
    println(orderOfLargestPlusSign(1, Array(Array(0, 0))))

  def orderOfLargestPlusSign(n: Int, mines: Array[Array[Int]]): Int =
    if mines.length == n * n then return 0

    val minesSet = Set.from(mines.map(m => (m.head, m.last)))
    val left     = Array.tabulate[Int](n, n) { case (x, y) => if minesSet.contains((x, y)) then 0 else 1 }
    val right    = Array.tabulate[Int](n, n) { case (x, y) => if minesSet.contains((x, y)) then 0 else 1 }
    val above    = Array.tabulate[Int](n, n) { case (x, y) => if minesSet.contains((x, y)) then 0 else 1 }
    val below    = Array.tabulate[Int](n, n) { case (x, y) => if minesSet.contains((x, y)) then 0 else 1 }

    var maxOrder = 1

    (1 until n - 1).foreach { x =>
      (1 until n - 1).foreach { y =>
        left(x)(y) = if minesSet.contains((x, y)) then 0 else left(x - 1)(y) + 1
        above(x)(y) = if minesSet.contains((x, y)) then 0 else above(x)(y - 1) + 1
      }
    }

    (n - 2 to 1 by -1).foreach { x =>
      (n - 2 to 1 by -1).foreach { y =>
        below(x)(y) = if minesSet.contains((x, y)) then 0 else below(x + 1)(y) + 1
        right(x)(y) = if minesSet.contains((x, y)) then 0 else right(x)(y + 1) + 1

        maxOrder = math.max(maxOrder, math.min(below(x)(y), math.min(above(x)(y), math.min(left(x)(y), right(x)(y)))))
      }
    }

    maxOrder
