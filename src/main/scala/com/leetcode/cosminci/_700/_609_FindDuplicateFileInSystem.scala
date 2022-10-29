package com.leetcode.cosminci._700

object _609_FindDuplicateFileInSystem:

  def findDuplicate(paths: Array[String]): List[List[String]] =
    val regex = "(.+)\\((.+)\\)".r
    def parse(line: String) =
      line.split(' ').toSeq match
        case dir +: files =>
          files.map { case regex(file, content) => content -> s"$dir/$file" }

    paths
      .flatMap(parse)
      .groupMap { case (content, _) => content } { case (_, path) => path }
      .values
      .collect { case group if group.length > 1 => group.toList }
      .toList
