package io.github.cosminci.leetcode._100

object _37_SudokuSolver:
  def main(args: Array[String]): Unit =
    val board = Array(
      Array('5', '3', '.', '.', '7', '.', '.', '.', '.'),
      Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
      Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
      Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
      Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
      Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
      Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
      Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
      Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
    )
    solveSudoku(board)
    print("")

  private def solveSudoku(board: Array[Array[Char]]): Unit =
    def solve(prevRow: Int, prevCol: Int): Boolean =
      var col = prevCol // because it needs reset after finishing row
      (prevRow until 9).foreach { r =>
        (col until 9).foreach { c =>
          if board(r)(c) == '.' then
            ('1' to '9').foreach { v =>
              if isValid(r, c, v) then
                board(r)(c) = v
                if solve(r, c + 1) then return true
                else board(r)(c) = '.'
            }
            return false
        }
        col = 0
      }
      true

    def isValid(row: Int, col: Int, value: Char): Boolean =
      val groupRow = row / 3 * 3
      val groupCol = col / 3 * 3
      (0 until 9).forall { idx =>
        board(idx)(col) != value &&
        board(row)(idx) != value &&
        board(groupRow + idx / 3)(groupCol + idx % 3) != value
      }

    solve(0, 0)
