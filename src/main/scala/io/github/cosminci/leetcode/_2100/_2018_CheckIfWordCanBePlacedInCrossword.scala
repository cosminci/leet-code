package io.github.cosminci.leetcode._2100

object _2018_CheckIfWordCanBePlacedInCrossword {
  def main(args: Array[String]): Unit = {
    println(placeWordInCrossword(Array(Array('#', ' ', '#'), Array(' ', ' ', '#'), Array('#', 'c', '#')), "abc"))
  }

  def placeWordInCrossword(board: Array[Array[Char]], word: String): Boolean = {
    val (m, n) = (board.length, board.head.length)
    val transposedBoard = (0 until n).map(c => board.map(_(c))).toArray

    Seq(board, transposedBoard).exists { b =>
      Seq(word, word.reverse).exists { w =>
        b.exists { row =>
          row.mkString.split('#').exists { slot =>
            slot.length == w.length &&
              slot.indices.forall(i => slot(i) == ' ' || slot(i) == w(i))
          }
        }
      }
    }
  }
}
