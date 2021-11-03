package io.github.cosminci.leetcode._100

object _81_SearchInRotatedSortedArrayII:
  def main(args: Array[String]): Unit =
    println(search(Array(2, 5, 6, 0, 0, 1, 2), 2))
    println(search(Array(2, 5), 2))

  def search(nums: Array[Int], target: Int): Boolean =
    if nums.length < 2 then return nums.contains(target)

    var (l, r) = (0, nums.length - 1)

    while l <= r do
      val mid = l + (r - l) / 2

      if target == nums(mid) then return true

      if nums(mid) == nums(l) then l += 1
      else if nums(mid) >= nums(l) == target >= nums(l) then
        if nums(mid) < target then l = mid + 1
        else r = mid - 1
      else if nums(mid) > nums(l) then l = mid + 1
      else r = mid - 1

    false
