package com.leetcode.cosminci._1700

object _1604_DesignParkingSpace:

  class ParkingSystem(big: Int, medium: Int, small: Int):

    private val parking = Array(big, medium, small)

    def addCar(carType: Int): Boolean =
      parking(carType - 1) -= 1
      parking(carType - 1) >= 0
