package io.github.cosminci.leetcode._2400

object _2383_MinHoursOfTrainingToWinCompetition:

  def minNumberOfHours(initialEnergy: Int, initialExperience: Int, energy: Array[Int], experience: Array[Int]): Int =
    (energy.sum - initialEnergy + 1).max(0) +
      experience
        .foldLeft((initialExperience, 0)) { case ((currXp, gap), requiredXp) =>
          if currXp > requiredXp then (currXp + requiredXp, gap)
          else (currXp + requiredXp + (requiredXp - currXp + 1), gap + (requiredXp - currXp + 1))
        }._2
