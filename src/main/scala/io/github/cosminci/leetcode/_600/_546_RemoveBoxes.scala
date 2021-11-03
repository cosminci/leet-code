package io.github.cosminci.leetcode._600

object _546_RemoveBoxes:
  def main(args: Array[String]): Unit =
    print(removeBoxes(Array(1, 3, 2, 2, 2, 3, 4, 3, 1)))

  def removeBoxes(boxes: Array[Int]): Int =
    val len = boxes.length
    val dp  = Array.ofDim[Int](len, len, len)

    def dfs(startIdx: Int, endIdx: Int, leftBoxesAttached: Int): Int =
      if startIdx > endIdx then return 0
      if dp(startIdx)(endIdx)(leftBoxesAttached) > 0 then return dp(startIdx)(endIdx)(leftBoxesAttached)

      dp(startIdx)(endIdx)(leftBoxesAttached) =
        dfs(startIdx, endIdx - 1, 0) + (leftBoxesAttached + 1) * (leftBoxesAttached + 1)
      (startIdx until endIdx).foreach { i =>
        if boxes(i) == boxes(endIdx) then
          dp(startIdx)(endIdx)(leftBoxesAttached) = math.max(
            dp(startIdx)(endIdx)(leftBoxesAttached),
            dfs(startIdx, i, leftBoxesAttached + 1) + dfs(i + 1, endIdx - 1, 0)
          )
      }
      dp(startIdx)(endIdx)(leftBoxesAttached)

    dfs(0, len - 1, 0)
