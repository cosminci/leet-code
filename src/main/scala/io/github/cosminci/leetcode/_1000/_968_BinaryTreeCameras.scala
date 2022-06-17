package io.github.cosminci.leetcode._1000

import io.github.cosminci.utils.TreeNode

object _968_BinaryTreeCameras:

  sealed trait State
  case object HasCamera    extends State
  case object IsCovered    extends State
  case object NeedsMonitor extends State

  def minCameraCover(root: TreeNode): Int =
    def dfs(node: TreeNode): (Int, State) =
      if node == null then (0, IsCovered)
      else
        val (leftCams, leftState)   = dfs(node.left)
        val (rightCams, rightState) = dfs(node.right)
        if leftState == NeedsMonitor || rightState == NeedsMonitor then (1 + leftCams + rightCams, HasCamera)
        else if leftState == HasCamera || rightState == HasCamera then (leftCams + rightCams, IsCovered)
        else (leftCams + rightCams, NeedsMonitor)

    val (cams, state) = dfs(root)
    if state == NeedsMonitor then cams + 1 else cams
