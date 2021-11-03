package io.github.cosminci.leetcode._600

object _542_ZeroOneMatrix:
  def main(args: Array[String]): Unit =
    updateMatrix(
      Array(
        Array(1, 0, 1, 1, 0, 0, 1, 0, 0, 1),
        Array(0, 1, 1, 0, 1, 0, 1, 0, 1, 1),
        Array(0, 0, 1, 0, 1, 0, 0, 1, 0, 0),
        Array(1, 0, 1, 0, 1, 1, 1, 1, 1, 1),
        Array(0, 1, 0, 1, 1, 0, 0, 0, 0, 1),
        Array(0, 0, 1, 0, 1, 1, 1, 0, 1, 0),
        Array(0, 1, 0, 1, 0, 1, 0, 0, 1, 1),
        Array(1, 0, 0, 0, 1, 1, 1, 1, 0, 1),
        Array(1, 1, 1, 1, 1, 1, 1, 0, 1, 0),
        Array(1, 1, 1, 1, 0, 1, 0, 0, 1, 1)
      )
    ).foreach { row =>
      println(row.mkString(", "))
    }

  def updateMatrix(m: Array[Array[Int]]): Array[Array[Int]] =
    val numRows     = m.length
    val numCols     = m(0).length
    val maxDistance = numRows + numCols
    val distances   = m.clone()

    // check upper left distances
    m.indices.foreach { r =>
      m(r).indices.foreach { c =>
        distances(r)(c) =
          if m(r)(c) == 0 then 0
          else {
            val upper = Option.when(r >= 1)(distances(r - 1)(c)).getOrElse(maxDistance)
            val left  = Option.when(c >= 1)(distances(r)(c - 1)).getOrElse(maxDistance)
            1 + math.min(upper, left)
          }
      }
    }
    // check bottom right distances
    m.indices.reverse.foreach { r =>
      m(r).indices.reverse.foreach { c =>
        val right  = Option.when(r < numRows - 1)(distances(r + 1)(c)).getOrElse(maxDistance)
        val bottom = Option.when(c < numCols - 1)(distances(r)(c + 1)).getOrElse(maxDistance)
        distances(r)(c) = math.min(distances(r)(c), 1 + math.min(right, bottom))
      }
    }
    distances
