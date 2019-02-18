package org.romeo.adventofcode.day5


import org.romeo.adventofcode.common.Puzzle

import scala.annotation.tailrec

object DayFive {
  def main(args: Array[String]): Unit = {
    new DayFive().run()
  }
}

// This solution is brute force and it *sucks*
class DayFive extends Puzzle("https://adventofcode.com/2018/day/5/input") {

  case class Element(letter: Char) {
    def opposite():Element = if(letter.isUpper) Element(letter.toLower) else Element(letter.toUpper)
  }

  case class Polymer(elements: List[Element]) {
    def react(): Polymer = {
      Polymer(elements.foldLeft(List.empty[Element])((acc, nextElement) => {
        if(acc.isEmpty || acc.last.opposite() != nextElement) {
          acc :+ nextElement
        } else {
          acc.dropRight(1)
        }
      }))
    }
    def length(): Int = elements.length
    def removeAll(element: Element): Polymer = {
      Polymer(elements.filter(e => e != element && e != element.opposite()))
    }
  }

  def parseInput(input: String): List[Element] = {
    input.map(Element.apply).toList
  }

  override def validateInput(input: String): Unit = {
    assert(parseInput(input).nonEmpty)
  }


  override def solvePart1(input: String): String = {
    val initial = Polymer(parseInput(input))

    @tailrec
    def iter(prevLength: Int, polymer: Polymer): Polymer = {
      if(polymer.length() == prevLength) {
        polymer
      } else {
        val newpoly = polymer.react()
        iter(polymer.length(), newpoly)
      }
    }

    val finalPoly = iter(Int.MaxValue, initial)

    finalPoly.length().toString
  }

  override def solvePart2(input: String): String = {
    val initial = Polymer(parseInput(input))
    val removedUnits = ('A' to 'Z').map(c => initial.removeAll(Element(c)))

    @tailrec
    def iter(prevLength: Int, polymer: Polymer): Polymer = {
      if(polymer.length() == prevLength) {
        polymer
      } else {
        val newpoly = polymer.react()
        iter(polymer.length(), newpoly)
      }
    }

    val finalPolys = removedUnits.map(iter(Int.MaxValue, _))

    finalPolys.map(_.length()).min.toString
  }
}
