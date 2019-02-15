package org.romeo.adventofcode.day2

import org.romeo.adventofcode.common.Puzzle

object DayTwo {
  def main(args: Array[String]): Unit = {
    new DayTwo().run()
  }
}

class DayTwo extends Puzzle("https://adventofcode.com/2018/day/2/input") {

  def parseInput(input: String): List[String] = {
    input.split("\n").toList
  }

  override def validateInput(input: String): Unit = {
    assert(parseInput(input).nonEmpty)
  }

  def letterCount(s: String): Map[Char, Int] = {
    s.distinct.foldLeft(Map.empty[Char, Int])(
      (acc, letter) => acc + (letter  -> s.count(_ == letter))
    )
  }

  override def solvePart1(input: String): String = {
    val ids = parseInput(input)
    val letterCounts = ids.map(letterCount)

    val idsWith2OfALetter = letterCounts.count(_.values.toSet.contains(2))
    val idsWith3OfALetter = letterCounts.count(_.values.toSet.contains(3))

    val checksum = idsWith2OfALetter * idsWith3OfALetter
    checksum.toString
  }

  // two string match if they differ by only 1 character
  def matching(s1: String, s2: String): Boolean = {
    if (s1.isEmpty || s2.isEmpty) {
      false
    } else if(s1.head == s2.head) {
      matching(s1.tail, s2.tail)
    } else {
      s1.tail == s2.tail
    }
  }

  override def solvePart2(input: String): String = {
    val ids = parseInput(input)
    val hit = ids.combinations(2).find(l => matching(l(0), l(1)))
    hit.get(0).intersect(hit.get(1))
  }
}
