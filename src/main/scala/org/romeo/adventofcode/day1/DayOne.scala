package org.romeo.adventofcode.day1

import org.romeo.adventofcode.common.Puzzle

object DayOne {
  def main(args: Array[String]): Unit = {
    new DayOne().run()
  }
}

class DayOne extends Puzzle("https://adventofcode.com/2018/day/1/input") {

  def parseInput(input: String): List[Int] = {
    input.split("\n").map(_.toInt).toList
  }

  override def validateInput(input: String): Unit = {
    assert(parseInput(input).nonEmpty)
  }


  override def solvePart1(input: String): String = {
    parseInput(input).sum.toString
  }

  override def solvePart2(input: String): String = {
    val inputInts = parseInput(input)
    val infiniteInput = Stream.continually(inputInts.toStream).flatten
    val totals = infiniteInput.scan(0)(_ + _)
    val reduceToMap = totals.scanLeft((Set.empty[Int], 0)) {
      case ((totalsSoFar, checkTotal), nextTotal) => {
        (totalsSoFar + checkTotal, nextTotal)
      }
    }
    val setContains: (Set[Int], Int) => Boolean = (xs, x) => xs.contains(x)
    // drop the first 2 so it doesn't trigger on the first 0 in the list
    val result = reduceToMap.drop(2).find(setContains.tupled)
    result.get._2.toString
  }
}
