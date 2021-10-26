package io.github.cosminci.leetcode._700

object _688_KnightProbability:
  def main(args: Array[String]): Unit =
    println(knightProbability(3, 2, 0, 0))
    println(knightProbability(1, 0, 0, 0))

  private def knightProbability(n: Int, k: Int, row: Int, col: Int): Double =
    val boardProbabilities = Seq.tabulate(n)(r => Seq.tabulate(n)(c => if r == row && c == col then 1.0 else 0.0))

    def validJumps(r: Int, c: Int) =
      Seq(
        (r - 1, c - 2),
        (r - 2, c - 1),
        (r - 2, c + 1),
        (r - 1, c + 2),
        (r + 1, c + 2),
        (r + 2, c + 1),
        (r + 2, c - 1),
        (r + 1, c - 2)
      ).filter((nr, nc) => nr >= 0 && nr < n && nc >= 0 && nc < n)

    (1 to k)
      .foldLeft(boardProbabilities) { case (prevBoardProbabilities, _) =>
        val prevPositions = for
          r <- 0 until n
          c <- 0 until n
          if (prevBoardProbabilities(r)(c) > 0)
        yield (r, c)

        prevPositions.foldLeft(Seq.fill(n, n)(0.0)) { case (newBoard, (r, c)) =>
          validJumps(r, c).foldLeft(newBoard) { case (newBoard, (nr, nc)) =>
            newBoard.updated(nr, newBoard(nr).updated(nc, newBoard(nr)(nc) + prevBoardProbabilities(r)(c) / 8))
          }
        }
      }
      .map(_.sum)
      .sum
